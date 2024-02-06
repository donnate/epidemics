import argparse
import copy
import numpy as np
import pandas as pd
import time 
import numpy as np
import networkx as nx
import sys, os

sys.path.append('/scratch/midway3/cdonnat//epidemics_2/epidemics/python')
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
parser.add_argument('--min_clip', type=float, default=1e-4)
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
alpha_fp = args.alpha_fp
nb_folds = 5 
lambda_range = np.exp((np.arange(-4, 2, step=0.2)) * np.log(10))

def compute_threshold(sol, time, scenario, delta = 0.1, min_clip = 1e-3):
    A_inv = np.linalg.pinv(scenario['Gamma0'].todense())
    # Compute the Euclidean norm of each column of A_inv
    column_norms = np.linalg.norm(A_inv, axis=0)
    n_nodes = nx.number_of_nodes(scenario['G'])
    # Find the maximal norm
    rho = np.max(column_norms)
    Delta_p_1 =  np.sum(np.abs(scenario['Gamma'].T.dot(sol)))
    Delta_p_0 =  np.sum(np.abs(scenario['Gamma'].T.dot(sol))>min_clip)
    p_0 = np.sum(np.abs(sol) > min_clip)
    d_max = np.asarray(nx.degree(scenario['G']))[:,1].max()
    kappa = 1./(2 * np.sqrt(np.min([d_max, time])))
    thres =2 * np.sqrt(2) * rho * np.log(4 * n_nodes**2/delta)  * np.min([Delta_p_1, 
                                                                         Delta_p_0 *  2 * np.sqrt(2) * rho * np.log(4 * n_nodes**2/delta)/kappa**2  + 2 * p_0/n_nodes * np.log(4/delta)**2 ])
    return(thres)
        

success = pd.DataFrame(np.zeros((10, 5)),columns= ['exp', 'success-ours', 'length-ours', 'success-naive', 'length-naive'])
T_max = 30
#success = pd.DataFrame(np.zeros((10, 5)),columns= ['exp', 'success-ours', 'length-ours', 'success-naive', 'length-naive'])
for exp in np.arange(1, 10):
    print("Experiment nb " + str(exp))
    scenario = generate_scenario(n_nodes = n_nodes, 
                                    beta = beta, gamma=gamma,
                alpha_fp =alpha_fp, 
                n_init = n_init, steps = steps, type_graph =graph_type,
                p=p, m=m,
                seed = 1,
                epsilon=0.001, do_plot = False,
                min_clip=min_clip)
    print('Infected: ' + str(scenario['epidemic']['y_observed'].sum()))
    #### Add the Cross-validation procedure
    mst = nx.dfs_tree(scenario['G'])
    folds = {}
    paths = dict(nx.shortest_path_length(mst, source=0))
    for i in np.arange(nb_folds):
        folds[i] = [key for key, value in paths.items() if value % nb_folds == i]
    path = dict(nx.shortest_path_length(mst, source=0))
    #folds = get_folds(mst, path, n, nb_folds=nb_folds,
    #          plot_tree=False)
    ##### Run k-fold crosss validation
    results = np.zeros((len(lambda_range), nb_folds))
    for k, j in enumerate(folds.keys()):
        print(j)
        fold = folds[j]
        y_interpolated = copy.deepcopy(scenario['epidemic']['y_observed'])
        y_interpolated[fold] = 0
        y_test =  scenario['epidemic']['y_observed'][fold]
        for kk,lambda_ in enumerate(lambda_range):
            ssnal = SSNAL(gamma=lambda_, verbose=0)
            res_ssnal = ssnal.fit(X=y_interpolated.reshape((n_nodes,1)),
                weight_matrix=scenario['W'].todense(), save_centers=True)
            sol = res_ssnal.centers_.T
            sol = np.clip(sol, 0, 1).flatten()
            diff = np.abs(sol[fold] -  y_test)
            if np.sum(y_test) > 0:
                diff_pos =  np.mean(diff[y_test >0])
            else:
                diff_pos =  np.mean(diff)
            diff_neg =  np.mean(diff[y_test == 0 ])
            err = diff_pos + diff_neg
            #err = 2./ (1./diff_pos + 1./diff_neg)
            #err = diff_pos  ### focus solely on the positives
            #err = np.mean(np.abs(y_test-sol[fold]))
            #err = roc_auc_score(y_test, sol[fold], average='weighted')
            #err = auc(fpr, tpr)
            results[kk, k] = err
    results_agg  = np.mean(results, 1)
    index_lambda_best = np.argmin(results_agg)
    lambda_best = lambda_range[index_lambda_best]
    ssnal = SSNAL(gamma=lambda_best, verbose=0)
    res_ssnal = ssnal.fit(X=scenario['epidemic']['y_observed'].reshape((n_nodes,1)),
                weight_matrix=scenario['W'].todense(), save_centers=True)
    sol = res_ssnal.centers_.T
    sol = np.clip(sol, 0, 1).flatten()
    #sol[np.where(scenario['epidemic']['y_observed']  == 1)[0]] = 1
    ### Propagate solution
    current_p = sol
    current_p[np.where(current_p <min_clip)[0]] = 0
    current_p_observed = scenario['epidemic']['y_observed']
    
    columns = ['Experiment', 'node_init', 'time',
                'thres',
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
                'l2_obs_minus_proposed_neg']
    results_df = pd.DataFrame(columns=columns)
    beta_v = np.array([beta] * n_nodes)
    gamma_v = np.array([gamma] * n_nodes)
    
    #candidates = []
    #for u in np.where(scenario['epidemic']['y_observed']>0)[0]:
    #    candidates += nx.neighbors(scenario['G'], u)
    candidates = np.arange(n_nodes) #np.unique(candidates)
    increment = 0
    thres = compute_threshold(sol, time, scenario, delta = 0.8, min_clip = 1e-1)
    for time in np.arange(1, T_max):
        for node in candidates:
            #print([node, time])
             #### Diffuse procedure from node
            y_init = np.zeros(n_nodes)
            y_init[node] = 1
            mock_epidemic = simulate_epidemic(scenario['W'], 
                                              y_init, 
                                              beta_v, gamma_v,
                                              steps = time, 
                                              alpha_fp = 0.1, seed = 1,
                                              min_clip=0)
            #### Look at the error
            proposed_diffusion = mock_epidemic['true_p']
            temp_res = [exp, node, time, thres, 
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
                np.sum(np.square(scenario['epidemic']['y_observed'] - proposed_diffusion)[scenario['epidemic']['true_p'] < min_clip])
                ]
            results_df.loc[increment] = temp_res
            increment += 1
    
    selected = results_df.loc[np.where(results_df['l2_sol_minus_proposed'] < thres)[0], ['time', 'node_init']]
    selected2 = results_df.loc[np.where(results_df['l2_obs_minus_proposed'] < thres)[0], ['time', 'node_init']]
    success.iloc[exp] = [exp,
                         np.sum(selected.iloc[np.where(selected['time'] == steps)[0]]['node_init']  == ind[0]),
                         selected.shape[0],
                         np.sum(selected2.iloc[np.where(selected2['time'] == steps)[0]]['node_init']  == ind[0]),
                         selected2.shape[0]]
    success.to_csv('/scratch/midway3/cdonnat/epidemics_2/epidemics/python/experiments/results/adam' +  args.namefile + '.csv', index=False)



#results_df.loc[increment] = temp_res
