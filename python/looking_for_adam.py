import argparse
import copy
import numpy as np
import pandas as pd
import time 
import numpy as np
import networkx as nx
import sys, os

#sys.path.append('/scratch/midway3/cdonnat/epidemics_2/epidemics/python')
from simulate_epidemics import * 
from solvers import *


parser = argparse.ArgumentParser()
parser.add_argument('--namefile', type=str)
parser.add_argument('--seed', type=int)
parser.add_argument('--steps', type=int, default=40)
parser.add_argument('--n_step_predict', type=int, default=15)
parser.add_argument('--beta', type=float, default=0.99)
parser.add_argument('--gamma', type=float, default=0.001)
parser.add_argument('--alpha_fp', type=float, default=0.00)
parser.add_argument('--graph_type', type=str, default="ER")
parser.add_argument('--p', type=float, default=0.1)
parser.add_argument('--m', type=int, default=3)
parser.add_argument('--n_nodes', type=int, default=1000)
parser.add_argument('--min_clip', type=float, default=1e-1)
args = parser.parse_args()

steps = args.steps
n_step_predict = args.n_step_predict
n_init = 1
n_nodes = args.n_nodes
graph_type = args.graph_type
p = args.p
m = args.m
beta = args.beta
gamma = args.gamma
min_clip = args.min_clip
min_clip_thres = 0.1
alpha_fp = args.alpha_fp
nb_folds = 5
T_max = 50
lambda_range = np.exp((np.arange(-4, 2, step=0.2)) * np.log(10))

def compute_threshold(sol, rho,
                      scenario, delta = 0.1):
    #A_inv = np.linalg.pinv(scenario['Gamma0'].todense())
    # Compute the Euclidean norm of each column of A_inv
    #column_norms = np.linalg.norm(A_inv, axis=0)
    n_nodes = nx.number_of_nodes(scenario['G'])
    # Find the maximal norm
    #rho = np.max(column_norms)
    Delta_p_1 =  np.sum(np.abs(scenario['Gamma0'].T.dot(sol)))
    Delta_p_0 =  np.sum(np.abs(scenario['Gamma0'].T.dot(sol))>0)
    p_0 = np.sum(np.abs(sol) > 1e-2)
    d_max = np.asarray(nx.degree(scenario['G']))[:,1].max()
    kappa = np.max([1./(2 * np.sqrt(np.min([d_max, np.max([Delta_p_0,1])]))), 1])
    thres = 2 * np.sqrt(2) * rho * np.log(4 * n_nodes**2/delta)  * np.min([Delta_p_1, 
                                                                         Delta_p_0 *  2 * np.sqrt(2) * rho * np.log(4 * n_nodes**2/delta)/kappa**2])  + 2 * p_0/n_nodes * np.log(4/delta)**2 
    return(thres)

success = pd.DataFrame(np.zeros((1000, 45)),columns= ['exp', 'm',
    'beta', 'gamma', 'steps', 'thresh10',
                'thresh20',
                'thresh30',
                'thresh40',
                'thresh50' ,
                'thresh70',
                'thresh90',
                'thresh95' ,
    'success-ours10', 'length-ours10', 'success-naive10', 'length-naive10',
    'success-ours20', 'length-ours20', 'success-naive20', 'length-naive20',
    'success-ours30', 'length-ours30', 'success-naive30', 'length-naive30',
    'success-ours40', 'length-ours40', 'success-naive40', 'length-naive40',
    'success-ours50', 'length-ours50', 'success-naive50', 'length-naive50',
    'success-ours70', 'length-ours70', 'success-naive70', 'length-naive70',
    'success-ours90', 'length-ours90', 'success-naive90', 'length-naive90',
    'success-ours95', 'length-ours95', 'success-naive95', 'length-naive95'])

#success = pd.DataFrame(np.zeros((10, 5)),columns= ['exp', 'success-ours', 'length-ours', 'success-naive', 'length-naive'])
for exp in np.arange(0, 1000):
    print("Experiment nb " + str(exp))
    scenario = generate_scenario(n_nodes = n_nodes, 
                                    beta = beta, gamma=gamma,
                alpha_fp =alpha_fp, 
                n_init = n_init, steps = steps, type_graph =graph_type,
                p=p, m=m,
                seed = args.seed *1000 + exp,
                epsilon=0.001, do_plot = False,
                min_clip=0)
    W_binary = nx.adjacency_matrix(scenario['G'], weight=None)
    #nb = np.sum( (sol > 1e-1 ) ) + np.sum((proposed_diffusion > 1e-1))  -np.sum(  (sol > 1e-1 ) * (proposed_diffusion > 1e-2))
    A_inv = np.linalg.pinv(scenario['Gamma0'].todense())
    # Compute the Euclidean norm of each column of A_inv
    column_norms = np.linalg.norm(A_inv, axis=0)
    # Find the maximal norm
    rho = np.max(column_norms)
    print('Infected: ' + str(scenario['epidemic']['y_observed'].sum()))
    #### Add the Cross-validation procedure
    y_folds = np.zeros((n_nodes,nb_folds))
    for fold in np.arange(nb_folds):
        y_folds[:, fold] = np.random.binomial(n=1, p=scenario['epidemic']['y_observed']/nb_folds)
    #path = dict(nx.shortest_path_length(mst, source=0))
    #folds = get_folds(mst, path, n, nb_folds=nb_folds,
    #          plot_tree=False)
    ##### Run k-fold crosss validation
    results = np.zeros((len(lambda_range), nb_folds))
    for fold in np.arange(nb_folds):
        print(fold)
        #fold = folds[j]
        #y_interpolated = copy.deepcopy(scenario['epidemic']['y_observed'])
        #y_interpolated[fold] = 0
        #y_test =  scenario['epidemic']['y_observed'][fold]
        y_test = y_folds[:, fold]
        y_interpolated = ( np.sum(y_folds, 1) - y_folds[:, fold]) / (nb_folds-1)
        for kk,lambda_ in enumerate(lambda_range):
            ssnal = SSNAL(gamma=lambda_, verbose=0)
            res_ssnal = ssnal.fit(X=y_interpolated.reshape((n_nodes,1)),
                weight_matrix=W_binary.todense(), save_centers=True)
            sol = res_ssnal.centers_.T
            sol = np.clip(sol, min_clip, 1).flatten()
            results[kk, fold] = np.mean(np.square(y_test - sol))
    results_agg  = np.mean(results, 1)
    index_lambda_best = np.argmin(results_agg)
    lambda_best = lambda_range[index_lambda_best]
    ssnal = SSNAL(gamma=lambda_best, verbose=0)
    res_ssnal = ssnal.fit(X=scenario['epidemic']['y_observed'].reshape((n_nodes,1)),
                weight_matrix=W_binary.todense(), save_centers=True)
    sol = res_ssnal.centers_.T
    sol = np.clip(sol, 0, 1).flatten()
    print([np.max(sol), np.min(sol)])
    #sol[sol<1e-1] = 0

    columns = ['Experiment', 'node_init', 'time',
               'graph_type',
                'n_nodes',
                'alpha_fp',
                'p',  'm', 'n_infected', 'w',
                'steps',
                'n_step_predict',
                'Lambda', 'final_number_infected',
                'l1_sol_minus_proposed',
                'l1_sol_minus_proposed_pos',
                'l1_sol_minus_proposed_neg',
                'l1_obs_minus_proposed',
                'l1_obs_minus_proposed_pos',
                'l1_obs_minus_proposed_neg',
                'l2_sol_minus_proposed',
                'l2_sol_minus_proposed_pos',
                'l2_sol_minus_proposed_neg',
                'l2_obs_minus_proposed',
                'l2_obs_minus_proposed_pos',
                'l2_obs_minus_proposed_neg',
                'thresh10',
                'thresh20',
                'thresh30',
                'thresh40',
                'thresh50',
                'thresh70',
                'thresh90',
                'thresh95'
              ]
    results_df = pd.DataFrame(columns=columns)
    beta_v = np.array([beta] * n_nodes)
    gamma_v = np.array([gamma] * n_nodes)
    candidates = np.arange(n_nodes) #np.unique(candidates)
    increment = 0
    for time in np.arange(1, T_max):
        for node in candidates:
            print([node, time])
             #### Diffuse procedure from node
            y_init = np.zeros(n_nodes)
            y_init[node] = 1
            mock_epidemic = simulate_epidemic(scenario['W'], 
                                              y_init, 
                                              beta_v, gamma_v,
                                              steps = time, 
                                              alpha_fp = alpha_fp, seed = 1,
                                              min_clip=0)
            #### Look at the error
            proposed_diffusion = mock_epidemic['true_p']
            proposed_diffusion[proposed_diffusion < 5 * 1e-2] = 0 
            T = np.max([(scenario['Gamma'].T.dot(proposed_diffusion) > min_clip_thres).sum(), 1])
            thres10 = np.min([compute_threshold(proposed_diffusion, rho, scenario, delta = 0.1),
                              compute_threshold(sol, rho, scenario, delta = 0.1)])
            thres20 = np.min([compute_threshold(proposed_diffusion, rho, scenario, delta = 0.2),
                               compute_threshold(sol, rho, scenario, delta = 0.2)])
            thres30 = np.min([compute_threshold(proposed_diffusion, rho, scenario, delta = 0.3),
                               compute_threshold(sol, rho, scenario, delta = 0.3)])
            thres40 = np.min([compute_threshold(proposed_diffusion, rho, scenario, delta = 0.4),
                               compute_threshold(sol, rho, scenario, delta = 0.4)])
            thres50 = np.min([compute_threshold(proposed_diffusion, rho, scenario, delta = 0.5),
                               compute_threshold(sol, rho, scenario, delta = 0.5)])
            thres70 = np.min([compute_threshold(proposed_diffusion, rho, scenario, delta = 0.7),
                               compute_threshold(sol, rho, scenario, delta = 0.7)])
            thres90 = np.min([compute_threshold(proposed_diffusion, rho, scenario, delta = 0.9),
                               compute_threshold(sol, rho, scenario, delta = 0.9)])
            thres95 = np.min([compute_threshold(proposed_diffusion, rho, scenario, delta = 0.95),
                               compute_threshold(sol, rho, scenario, delta = 0.95)])
            temp_res = [exp, node, time, 
                graph_type, n_nodes,
                alpha_fp,
                p, m, scenario['epidemic']['y_true'].sum(),
                scenario['W'].max(),
                steps, n_step_predict,
                lambda_best, 
                scenario['epidemic']['y_observed'].sum(),
                np.mean(np.abs(sol - proposed_diffusion)),
                np.mean(np.abs(sol - proposed_diffusion)[scenario['epidemic']['true_p'] > min_clip]),
                np.mean(np.abs(sol - proposed_diffusion)[scenario['epidemic']['true_p'] < min_clip]),
                #np.mean(np.abs(sol - scenario['epidemic']['true_p'])[scenario['epidemic']['true_p'] < 0]),
                np.mean(np.abs(scenario['epidemic']['y_observed'] - proposed_diffusion)),
                np.mean(np.abs(scenario['epidemic']['y_observed'] - proposed_diffusion)[scenario['epidemic']['true_p'] > min_clip]),
                np.mean(np.abs(scenario['epidemic']['y_observed'] - proposed_diffusion)[scenario['epidemic']['true_p'] < min_clip]),
                np.sum(np.square(sol - proposed_diffusion)),
                np.sum(np.square(sol - proposed_diffusion)[scenario['epidemic']['true_p'] > min_clip]),
                np.sum(np.square(sol - proposed_diffusion)[scenario['epidemic']['true_p'] < min_clip]),
                #np.mean(np.abs(sol - scenario['epidemic']['true_p'])[scenario['epidemic']['true_p'] < 0]),
                np.sum(np.square(scenario['epidemic']['y_observed'] - proposed_diffusion)),
                np.sum(np.square(scenario['epidemic']['y_observed'] - proposed_diffusion)[scenario['epidemic']['true_p'] > min_clip]),
                np.sum(np.square(scenario['epidemic']['y_observed'] - proposed_diffusion)[scenario['epidemic']['true_p'] < min_clip]),
                thres10, thres20, thres30, thres40, thres50, thres70, thres90, thres95
                ]
            print( [time, node, min_clip_thres, 
                np.sum(np.square(sol - proposed_diffusion)),
                np.sum(np.square(scenario['epidemic']['y_observed'] - proposed_diffusion)),
                thres10, thres20, thres30, thres40, thres50
                ])
            #print(len(temp_res))
            #print(results_df.shape)
            results_df.loc[increment] = temp_res
            increment += 1
    results_df.to_csv('~/Documents/epidemic_modelling/python/experiments/results/intermediary_adam' +  args.namefile + '.csv', index=False)
    selected10 = results_df.loc[np.where(results_df['l2_sol_minus_proposed'] < results_df['thresh10'])[0], ['time', 'node_init']]
    selected10_obs = results_df.loc[np.where(results_df['l2_obs_minus_proposed'] < results_df['thresh10'])[0], ['time', 'node_init']]
    selected20 = results_df.loc[np.where(results_df['l2_sol_minus_proposed'] < results_df['thresh20'])[0], ['time', 'node_init']]
    selected20_obs = results_df.loc[np.where(results_df['l2_obs_minus_proposed'] < results_df['thresh20'])[0], ['time', 'node_init']]
    selected30 = results_df.loc[np.where(results_df['l2_sol_minus_proposed'] < results_df['thresh30'])[0], ['time', 'node_init']]
    selected30_obs = results_df.loc[np.where(results_df['l2_obs_minus_proposed'] < results_df['thresh30'])[0], ['time', 'node_init']]
    selected40 = results_df.loc[np.where(results_df['l2_sol_minus_proposed'] < results_df['thresh40'])[0], ['time', 'node_init']]
    selected40_obs = results_df.loc[np.where(results_df['l2_obs_minus_proposed'] < results_df['thresh40'])[0], ['time', 'node_init']]
    selected50 = results_df.loc[np.where(results_df['l2_sol_minus_proposed'] < results_df['thresh50'])[0], ['time', 'node_init']]
    selected50_obs = results_df.loc[np.where(results_df['l2_obs_minus_proposed'] < results_df['thresh50'])[0], ['time', 'node_init']]
    selected70 = results_df.loc[np.where(results_df['l2_sol_minus_proposed'] < results_df['thresh70'])[0], ['time', 'node_init']]
    selected70_obs = results_df.loc[np.where(results_df['l2_obs_minus_proposed'] < results_df['thresh70'])[0], ['time', 'node_init']]
    selected90 = results_df.loc[np.where(results_df['l2_sol_minus_proposed'] < results_df['thresh90'])[0], ['time', 'node_init']]
    selected90_obs = results_df.loc[np.where(results_df['l2_obs_minus_proposed'] < results_df['thresh90'])[0], ['time', 'node_init']]
    selected95 = results_df.loc[np.where(results_df['l2_sol_minus_proposed'] < results_df['thresh95'])[0], ['time', 'node_init']]
    selected95_obs = results_df.loc[np.where(results_df['l2_obs_minus_proposed'] < results_df['thresh95'])[0], ['time', 'node_init']]
    ind = np.where(scenario['y_init'] == 1)[0]
    
    success.iloc[exp] = [exp, m, beta, gamma, steps,
                        np.mean(results_df['thresh10']),
                        np.mean(results_df['thresh20']),
                        np.mean(results_df['thresh30']),
                        np.mean(results_df['thresh40']),
                        np.mean(results_df['thresh50']),
                        np.mean(results_df['thresh70']),
                        np.mean(results_df['thresh90']),
                        np.mean(results_df['thresh95']),
                         np.sum(selected10.iloc[np.where(selected10['time'] == steps)[0]]['node_init']  == ind[0]),
                         selected10.shape[0]/results_df.shape[0],
                         np.sum(selected10_obs.iloc[np.where(selected10_obs['time'] == steps)[0]]['node_init']  == ind[0]),
                         selected10_obs.shape[0]/results_df.shape[0],
                         np.sum(selected20.iloc[np.where(selected20['time'] == steps)[0]]['node_init']  == ind[0]),
                         selected20.shape[0]/results_df.shape[0],
                         np.sum(selected20_obs.iloc[np.where(selected20_obs['time'] == steps)[0]]['node_init']  == ind[0]),
                         selected20_obs.shape[0]/results_df.shape[0],
                         np.sum(selected30.iloc[np.where(selected30['time'] == steps)[0]]['node_init']  == ind[0]),
                         selected30.shape[0]/results_df.shape[0],
                         np.sum(selected30_obs.iloc[np.where(selected30_obs['time'] == steps)[0]]['node_init']  == ind[0]),
                         selected30_obs.shape[0]/results_df.shape[0],
                         np.sum(selected40.iloc[np.where(selected40['time'] == steps)[0]]['node_init']  == ind[0]),
                         selected40.shape[0]/results_df.shape[0],
                         np.sum(selected40_obs.iloc[np.where(selected40_obs['time'] == steps)[0]]['node_init']  == ind[0]),
                         selected40_obs.shape[0]/results_df.shape[0],
                         np.sum(selected50.iloc[np.where(selected50['time'] == steps)[0]]['node_init']  == ind[0]),
                         selected50.shape[0]/results_df.shape[0],
                         np.sum(selected50_obs.iloc[np.where(selected50_obs['time'] == steps)[0]]['node_init']  == ind[0]),
                         selected50_obs.shape[0]/results_df.shape[0],
                        np.sum(selected70.iloc[np.where(selected70['time'] == steps)[0]]['node_init']  == ind[0]),
                         selected70.shape[0]/results_df.shape[0],
                        np.sum(selected70_obs.iloc[np.where(selected70_obs['time'] == steps)[0]]['node_init']  == ind[0]),
                         selected70_obs.shape[0]/results_df.shape[0],
                        np.sum(selected90.iloc[np.where(selected90['time'] == steps)[0]]['node_init']  == ind[0]),
                        selected90.shape[0]/results_df.shape[0],
                        np.sum(selected90_obs.iloc[np.where(selected90_obs['time'] == steps)[0]]['node_init']  == ind[0]),
                         selected90_obs.shape[0]/results_df.shape[0],
                        np.sum(selected95.iloc[np.where(selected95['time'] == steps)[0]]['node_init']  == ind[0]),
                         selected95.shape[0]/results_df.shape[0],
                         np.sum(selected95_obs.iloc[np.where(selected95_obs['time'] == steps)[0]]['node_init']  == ind[0]),
                         selected95_obs.shape[0]/results_df.shape[0]
                         ]

    success.to_csv('~/Documents/epidemic_modelling/python/experiments/results/adam' +  args.namefile + '.csv', index=False)



