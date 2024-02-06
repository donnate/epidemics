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



success = pd.DataFrame(np.zeros((10, 9)),columns= ['exp',
    'beta', 'gamma', 'm', 'steps', 'beta-ours', 'gamma-ours',
      'beta-naive', 'gamma-naive',
      'beta-oracle', 'gamma-oracle'])
K = 10
for experiment in np.arange(10):
    scenario = generate_scenario(n_nodes = n_nodes, 
                                    beta = beta, gamma=gamma,
                alpha_fp =alpha_fp, 
                n_init = n_init, steps = steps + K, type_graph =graph_type,
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
    solution = np.zeros((K, n_nodes))
    for k in np.arange(K):
        results = np.zeros((len(lambda_range), nb_folds))
        for it_j, j in enumerate(folds.keys()):
            print(j)
            fold = folds[j]
            y_interpolated = copy.deepcopy(scenario['epidemic']['realized_state'][:, steps + k])
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
                results[kk, it_j] = err
        results_agg  = np.mean(results, 1)
        index_lambda_best = np.argmin(results_agg)
        lambda_best = lambda_range[index_lambda_best]
        ssnal = SSNAL(gamma=lambda_best, verbose=0)
        res_ssnal = ssnal.fit(X=scenario['epidemic']['realized_state'][:,steps + k].reshape((n_nodes,1)),
                    weight_matrix=scenario['W'].todense(), save_centers=True)
        sol_temp = res_ssnal.centers_.T
        sol_temp = np.clip(sol_temp, 0.005, 1).flatten()
        #sol_temp[np.where(scenario['epidemic']['y_observed']  == 1)[0]] = 1
        solution[k,:] =  sol_temp
        ### Propagate solution
    ###
    Delta_p = np.zeros((K, 1))
    Phi = np.zeros((K, 2))
    for k in np.arange(K):
        Delta_p[k,:] =  np.sum(solution[k+1,:] - solution[k,:])
        Phi[k,0] = np.sum(np.diag(np.array([1] * n_nodes) - solution[k,:]).dot(scenario['W'] .dot(solution[k,:]))) 
        Phi[k,1] = -np.sum(solution[k,:]) 
    estimated_params = np.linalg.pinv(Phi).dot(Delta_p)
    error_beta_ours = estimated_params[0] - beta
    error_gamma_ours = estimated_params[1] - gamma

    Delta_p = np.zeros((K, 1))
    Phi = np.zeros((K, 2))
    for k in np.arange(K):
        Delta_p[k,:] =    np.sum(scenario['epidemic']['realized_state'][k+1,:] - scenario['epidemic']['realized_state'][k,:])
        Phi[k,0] = np.sum(np.diag(np.array([1] * n_nodes) - scenario['epidemic']['realized_state'][:,k]).dot(scenario['W'] .dot(scenario['epidemic']['realized_state'][:,k]))) 
        Phi[k,1] = -np.sum(scenario['epidemic']['realized_state'][:,k]) 
    estimated_params = np.linalg.pinv(Phi).dot(Delta_p)
    error_beta_naive= estimated_params[0] - beta
    error_gamma_naive = estimated_params[1] - gamma


    Delta_p = np.zeros((K, 1))
    Phi = np.zeros((K, 2))
    for k in np.arange(K):
        Delta_p[k,:] =  np.sum(scenario['epidemic']['track_state'][k+1,:] - scenario['epidemic']['track_state'][k,:])
        Phi[k,0] = np.sum(np.diag(np.array([1] * n_nodes) - scenario['epidemic']['track_state'][:,k]).dot(scenario['W'] .dot(scenario['epidemic']['track_state'][:,k]))) 
        Phi[k,1] = -np.sum(scenario['epidemic']['track_state'][:,k]) 
    estimated_params = np.linalg.pinv(Phi).dot(Delta_p)
    error_beta_oracle = estimated_params[0] - beta
    error_gamma_oracle = estimated_params[1] - gamma
    success.iloc[experiment] = [experiment, m, beta, gamma, steps,
                        error_beta_ours, error_gamma_ours,
                        error_beta_naive, error_gamma_naive,
                        error_beta_oracle, error_gamma_oracle
                        ]
    success.to_csv('/scratch/midway3/cdonnat/epidemics_2/epidemics/python/experiments/results/param' +  args.namefile + '.csv', index=False)



