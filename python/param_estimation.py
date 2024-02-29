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
parser.add_argument('--steps', type=int, default=40)
parser.add_argument('--n_step_predict', type=int, default=15)
parser.add_argument('--beta', type=float, default=0.99)
parser.add_argument('--gamma', type=float, default=0.001)
parser.add_argument('--alpha_fp', type=float, default=0.00)
parser.add_argument('--graph_type', type=str, default="ER")
parser.add_argument('--p', type=float, default=0.1)
parser.add_argument('--m', type=int, default=3)
parser.add_argument('--n_nodes', type=int, default=1000)
parser.add_argument('--K', type=int, default=3)
parser.add_argument('--min_clip', type=float, default=1e-4)
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
lambda_range = [1e-4, 0.005, 0.001, 0.005, 0.01, 0.05,  0.1, 0.25, 0.5, 0.75, 1, 2, 3, 5, 10, 
                        15, 20, 30, 50, 80, 100] #np.exp((np.arange(-4, 2, step=0.2)) * np.log(10))



success = pd.DataFrame(np.zeros((100,17)),columns= ['exp',
   'm', 'beta', 'gamma', 'steps', 'err-beta-ours', 'err-gamma-ours',
      'err-beta-naive', 'err-gamma-naive',
      'err-beta-oracle', 'err-gamma-oracle',
      'beta-ours', 'gamma-ours',
      'beta-naive', 'gamma-naive',
      'beta-oracle', 'gamma-oracle'])
for experiment_nb in np.arange(100):
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
        print("k = " + str(k))
        results = np.zeros((len(lambda_range), nb_folds))
        y_folds = np.zeros((n_nodes,nb_folds))
        for fold in np.arange(nb_folds):
            y_folds[:, fold] = np.random.binomial(n=1, p=scenario['epidemic']['realized_state'][:,steps + k].reshape((n_nodes,))/nb_folds)
        for fold in np.arange(nb_folds):
            y_test = y_folds[:, fold]
            y_interpolated = ( np.sum(y_folds, 1) - y_folds[:, fold]) / (nb_folds-1)
            for kk,lambda_ in enumerate(lambda_range):
                ssnal = SSNAL(gamma=lambda_, verbose=0)
                res_ssnal = ssnal.fit(X=y_interpolated.reshape((n_nodes,1)),
                    weight_matrix=scenario['W'].todense(), save_centers=True)
                sol = res_ssnal.centers_.T
                sol = np.clip(sol, 0, 1).flatten()
                results[kk, fold] = np.mean(np.square((y_test, sol)))
        
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



