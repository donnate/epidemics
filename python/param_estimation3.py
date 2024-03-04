import argparse
import copy
import numpy as np
import pandas as pd
import time 
import numpy as np
import networkx as nx
import sys, os

sys.path.append('~/Documents/epidemic_modelling/python')
from simulate_epidemics import * 
from solvers import *


parser = argparse.ArgumentParser()
parser.add_argument('--namefile', type=str)
parser.add_argument('--seed', type=int)
parser.add_argument('--steps', type=int, default=20)
parser.add_argument('--n_step_predict', type=int, default=15)
parser.add_argument('--beta', type=float, default=0.5)
parser.add_argument('--gamma', type=float, default=0.1)
parser.add_argument('--alpha_fp', type=float, default=0.00)
parser.add_argument('--graph_type', type=str, default="knn")
parser.add_argument('--p', type=float, default=0.1)
parser.add_argument('--m', type=int, default=5)
parser.add_argument('--n_nodes', type=int, default=1000)
parser.add_argument('--K', type=int, default=10)
parser.add_argument('--min_clip', type=float, default=0)
args = parser.parse_args()

steps = args.steps
n_step_predict = args.n_step_predict
n_init = 1
seed = args.seed
n_nodes = args.n_nodes
graph_type = args.graph_type
p = args.p
m = args.m
K = args.K
beta = args.beta
gamma = args.gamma
min_clip = args.min_clip
alpha_fp = args.alpha_fp
nb_folds = 2
collection_lambda_best = {(2, 0.2, 10 ) : 0.25,
               (2, 0.2, 20 ) : 0.25,
               (2, 0.2, 30 ) : 0.25,
               (2, 0.35, 10 ) : 0.25,
               (2, 0.35, 20 ) : 0.25,
               (2, 0.35, 30 ) : 0.5,
               (2, 0.5, 10 ) : 0.25,
               (2, 0.5, 20 ) : 0.5,
               (2, 0.5, 30 ) : 0.5,
               (2, 0.65, 10 ) : 0.25,
               (2, 0.65, 20 ) : 0.5,
               (2, 0.65, 30 ) : 0.5,
               (2, 0.8, 10 ) : 0.25,
               (2, 0.8, 20 ) : 0.5,
               (2, 0.8, 30 ) : 0.5,
               (2, 0.9, 10 ) : 0.25,
               (2, 0.9, 20 ) : 0.5,
               (2, 0.9, 30 ) : 0.75,
               (5, 0.2, 10 ) : 0.1,
               (5, 0.2, 20 ) : 0.1,
               (5, 0.2, 30 ) : 0.1,
               (5, 0.35, 10 ) : 0.1,
               (5, 0.35, 20 ) : 0.1,
               (5, 0.35, 30 ) : 0.25,
               (5, 0.5, 10 ) : 0.1,
               (5, 0.5, 20 ) : 0.25,
               (5, 0.5, 30 ) : 0.25,
               (5, 0.65, 10 ) : 0.1,
               (5, 0.65, 20 ) : 0.25,
               (5, 0.65, 30 ) : 0.25,
               (5, 0.8, 10 ) : 0.1,
               (5, 0.8, 20 ) : 0.25,
               (5, 0.8, 30 ) : 0.25,
               (5, 0.9, 10 ) : 0.1,
               (5, 0.9, 20 ) : 0.25,
               (5, 0.9, 30 ) : 0.25,
               (7, 0.2, 10 ) : 0.05,
               (7, 0.2, 20 ) : 0.1,
               (7, 0.2, 30 ) : 0.1,
               (7, 0.35, 10 ) : 0.1,
               (7, 0.35, 20 ) : 0.1,
               (7, 0.35, 30 ) : 0.1,
               (7, 0.5, 10 ) : 0.1,
               (7, 0.5, 20 ) : 0.1,
               (7, 0.5, 30 ) : 0.1,
               (7, 0.65, 10 ) : 0.1,
               (7, 0.65, 20 ) : 0.1,
               (7, 0.65, 30 ) : 0.1,
               (7, 0.8, 10 ) : 0.1,
               (7, 0.8, 20 ) : 0.1,
               (7, 0.8, 30 ) : 0.1,
               (7, 0.9, 10 ) : 0.1,
               (7, 0.9, 20 ) : 0.1,
               (7, 0.9, 30 ) : 0.1,
               (10, 0.2, 10 ) : 0.05,
               (10, 0.2, 20 ) : 0.05,
               (10, 0.2, 30 ) : 0.1,
               (10, 0.35, 10 ) : 0.05,
               (10, 0.35, 20 ) : 0.1,
               (10, 0.35, 30 ) : 0.1,
               (10, 0.5, 10 ) : 0.05,
               (10, 0.5, 20 ) : 0.1,
               (10, 0.5, 30 ) : 0.1,
               (10, 0.65, 10 ) : 0.1,
               (10, 0.65, 20 ) : 0.1,
               (10, 0.65, 30 ) : 0.1,
               (10, 0.8, 10 ) : 0.1,
               (10, 0.8, 20 ) : 0.1,
               (10, 0.8, 30 ) : 0.1,
               (10, 0.9, 10 ) : 0.1,
               (10, 0.9, 20 ) : 0.1,
               (10, 0.9, 30 ) : 0.1
                }


success = pd.DataFrame(np.zeros((100,17)),columns= ['exp',
   'm', 'beta', 'gamma', 'steps', 'err-beta-ours', 'err-gamma-ours',
      'err-beta-naive', 'err-gamma-naive',
      'err-beta-oracle', 'err-gamma-oracle',
      'beta-ours', 'gamma-ours',
      'beta-naive', 'gamma-naive',
      'beta-oracle', 'gamma-oracle'])
for experiment_nb in np.arange(500):
    scenario = generate_scenario(n_nodes = n_nodes, 
                                    beta = beta, gamma=gamma,
                alpha_fp =alpha_fp, 
                n_init = n_init, steps = steps + K, type_graph =graph_type,
                p=p, m=m,
                seed = int(seed * 1000 + experiment_nb),
                epsilon=0.001, do_plot = False,
                min_clip=min_clip)
    print('Infected: ' + str(scenario['epidemic']['y_observed'].sum()))
    #### Add the Cross-validation procedure
    solution = np.zeros((K, n_nodes))
    for k in np.arange(K):
        lambda_best = collection_lambda_best[(m, beta, steps)]
        ssnal = SSNAL(gamma=lambda_best, verbose=0)
        res_ssnal = ssnal.fit(X=scenario['epidemic']['realized_state'][:,steps + k].reshape((n_nodes,1)),
                    weight_matrix=scenario['W'].todense(), save_centers=True)
        sol_temp = res_ssnal.centers_.T
        sol_temp = np.clip(sol_temp, 0.005, 1).flatten()
        #sol_temp[np.where(scenario['epidemic']['y_observed']  == 1)[0]] = 1
        solution[k,:] =  sol_temp
        ### Propagate solution
    ###
    Delta_p = np.zeros((K-1, 1))
    Phi = np.zeros((K-1, 2))
    for k in np.arange(K-1):
        Delta_p[k,:] =  np.sum(solution[k+1,:] - solution[k,:])
        Phi[k,0] = np.sum(np.diag(np.array([1] * n_nodes) - solution[k,:]).dot(scenario['W'] .dot(solution[k,:]))) 
        Phi[k,1] = -np.sum(solution[k,:]) 
    estimated_params = np.linalg.pinv(Phi).dot(Delta_p)
    error_beta_ours = estimated_params[0] - beta
    error_gamma_ours = estimated_params[1] - gamma

    naive_solution  = scenario['epidemic']['realized_state'][:,(steps):(steps + K)].T
    Delta_p = np.zeros((K-1, 1))
    Phi = np.zeros((K-1, 2))
    naive_solution  = scenario['epidemic']['realized_state'][:,(steps):(steps + K)].T
    for k in np.arange(K-1):
        Delta_p[k,:] =  np.sum(naive_solution[k+1,:] - naive_solution[k,:])
        Phi[k,0] = np.sum(np.diag(np.array([1] * n_nodes) - naive_solution[k,:]).dot(scenario['W'] .dot(naive_solution[k,:])))
        Phi[k,1] = -np.sum(naive_solution[k,:])
    estimated_params_naive = np.linalg.pinv(Phi).dot(Delta_p)
    
    error_beta_naive= estimated_params_naive[0] - beta
    error_gamma_naive = estimated_params_naive[1] - gamma


    oracle_solution  = scenario['epidemic']['track_state'][:,(steps):(steps + K)].T
    Delta_p = np.zeros((K-1, 1))
    Phi = np.zeros((K-1, 2))
    for k in np.arange(K-1):
        Delta_p[k,:] =  np.sum(oracle_solution[k+1,:] - oracle_solution[k,:])
        Phi[k,0] = np.sum(np.diag(np.array([1] * n_nodes) - oracle_solution[k,:]).dot(scenario['W'] .dot(oracle_solution[k,:])))
        Phi[k,1] = -np.sum(oracle_solution[k,:])
    estimated_params_oracle = np.linalg.pinv(Phi).dot(Delta_p)
    
    error_beta_oracle = estimated_params_oracle[0] - beta
    error_gamma_oracle = estimated_params_oracle[1] - gamma
    print(error_beta_ours, error_gamma_ours)
    print(error_beta_naive, error_gamma_naive)
    print(error_beta_oracle, error_gamma_oracle)
    print([experiment_nb, m, beta, gamma, steps,
                        error_beta_ours[0], error_gamma_ours[0],
                        error_beta_naive[0], error_gamma_naive[0],
                        error_beta_oracle[0], error_gamma_oracle[0],
                        estimated_params[0][0], estimated_params[1][0],
                        estimated_params_naive[0][0], estimated_params_naive[1][0],
                        estimated_params_oracle[0][0], estimated_params_oracle[1][0]
                        ])
    success.iloc[experiment_nb] = [experiment_nb, m, beta, gamma, steps,
                        error_beta_ours[0], error_gamma_ours[0],
                        error_beta_naive[0], error_gamma_naive[0],
                        error_beta_oracle[0], error_gamma_oracle[0],
                        estimated_params[0][0], estimated_params[1][0],
                        estimated_params_naive[0][0], estimated_params_naive[1][0],
                        estimated_params_oracle[0][0], estimated_params_oracle[1][0]
                        ]
    success.to_csv('~/Documents/epidemic_modelling/python/experiments/results/param' +  args.namefile + '.csv', index=False)



