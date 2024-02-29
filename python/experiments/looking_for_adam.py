import argparse
import copy
import numpy as np
import pandas as pd
import time 
import numpy as np
import networkx as nx
import sys, os
sys.path.append('/scratch/midway3/cdonnat/epidemics/python')

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
T_max = 30
lambda_range = np.exp((np.arange(-4, 2, step=0.2)) * np.log(10))

columns = ['Experiment', 'Method', 'Time',
           'graph_type',
            'n_nodes',
            'alpha_fp',
            'p',  'm', 'n_infected', 'w',
            'steps',
            'n_step_predict',
            'Lambda', 'final_number_infected', 
            'Accuracy_true_p', 'Accuracy_true_y',
            'Accuracy_true_p_pos',
            'Accuracy_true_p_neg',  
            'bench_Accuracy_true_p',
            'bench_Accuracy_true_y']
for step in np.arange(n_step_predict):
    columns += ['accuracy_prop_' + str(step), 
                'Accuracy_true_p_pos_'  + str(step),
                'Accuracy_true_p_neg_'  + str(step), 
               'accuracy_benchmark_prop_' + str(step)]
results_df = pd.DataFrame(columns=columns)
increment = 0
for exp in np.arange(100):
    ### generate epidemic
    scenario = generate_scenario(n_nodes = n_nodes, 
                                    beta = beta, gamma=gamma,
                alpha_fp =alpha_fp, 
                n_init = n_init, steps = steps, type_graph =graph_type,
                p=p, m=m,
                seed = args.seed,
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
    sol[np.where(scenario['epidemic']['y_observed']  == 1)[0]] = 1
    ### Propagate solution
    
    temp_res = [exp, 'SSNAL-opt', end_time - start_time,  
                graph_type, n_nodes,
                args.alpha_fp,
                p, m, scenario['epidemic']['y_true'].sum(),
                scenario['W'].max(),
                args.steps, args.n_step_predict,
                lambda_best, 
                scenario['epidemic']['y_observed'].sum(),
                np.mean(np.abs(sol - scenario['epidemic']['true_p'])),
                np.mean(np.abs(sol  - scenario['epidemic']['y_true'])),
                np.mean(np.abs(sol - scenario['epidemic']['true_p'])[scenario['epidemic']['true_p'] > min_clip]),
                np.mean(np.abs(sol - scenario['epidemic']['true_p'])[scenario['epidemic']['true_p'] < min_clip]),
                #np.mean(np.abs(sol - scenario['epidemic']['true_p'])[scenario['epidemic']['true_p'] < 0]),
                np.mean(np.abs(scenario['epidemic']['y_observed'] - scenario['epidemic']['true_p'])),
                np.mean(np.abs(scenario['epidemic']['y_observed']  - scenario['epidemic']['y_true'])),
                ]
    current_p = sol
    current_p[np.where(current_p <min_clip)[0]] = 0
    current_p_observed = scenario['epidemic']['y_observed']
    results_df.loc[increment] = temp_res

    beta_v = np.array([beta] * n_nodes)
    gamma_v = np.array([gamma] * n_nodes)

    for t in np.arange(T_max):
        for node in np.arange(n_nodes):
             #### Diffuse procedure from node
            mock_epidemic = simulate_epidemic(scenario['W'], node, 
                                              beta_v, gamma_v,
                      steps = 1, 
                      alpha_fp = 0.1, seed = 1,
                      min_clip=0)
            #### Look at the error
            proposed_diffusion = mock_epidemic['epidemic']['true_p']
            temp_res = [exp, 
                graph_type, n_nodes,
                args.alpha_fp,
                p, m, scenario['epidemic']['y_true'].sum(),
                scenario['W'].max(),
                args.steps, args.n_step_predict,
                lambda_best, 
                scenario['epidemic']['y_observed'].sum(),
                np.mean(np.abs(sol - proposed_diffusion)),
                np.mean(np.abs(sol - proposed_diffusion)[scenario['epidemic']['true_p'] > min_clip]),
                np.mean(np.abs(sol - proposed_diffusion)[scenario['epidemic']['true_p'] < min_clip]),
                #np.mean(np.abs(sol - scenario['epidemic']['true_p'])[scenario['epidemic']['true_p'] < 0]),
                np.mean(np.abs(scenario['epidemic']['y_observed'] - proposed_diffusion)),
                np.mean(np.abs(scenario['epidemic']['y_observed'] - proposed_diffusion)[scenario['epidemic']['true_p'] > min_clip]),
                np.mean(np.abs(scenario['epidemic']['y_observed'] - proposed_diffusion)[scenario['epidemic']['true_p'] < min_clip]),
                ]

    

    #results_df.to_csv('/scratch/midway3/cdonnat/epidemics/python/experiments/results/binarized_new_results_algo_semi_synthetic' +  args.namefile + '.csv', index=False)
    results_df.to_csv('~/Downloads/binarized_new_results_algo_semi_synthetic' +  args.namefile + '.csv', index=False)

        

