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
                        15, 20, 30, 50, 80, 100]
m_range = [ 5, 10]
p_range = [0] 
step_range = [20, 30]

success = pd.DataFrame(np.zeros((500,22)),columns= ['exp',
   'm', 'beta', 'gamma', 'steps', 'K',
    'l1-error','l1-error-bench',
        'l1-erro-Kr','l1-error-K-bench',
      'err-beta-ours', 'err-gamma-ours',
      'err-beta-naive', 'err-gamma-naive',
      'err-beta-oracle', 'err-gamma-oracle',
      'beta-ours', 'gamma-ours',
      'beta-naive', 'gamma-naive',
      'beta-oracle', 'gamma-oracle'])
increment = 0
for exp in np.arange(500):
        for m in m_range:
            for p in p_range:
                for beta in [0.5, 0.7, 0.9, 0.2]:
                    for steps in step_range:
                        scenario = generate_scenario(n_nodes = n_nodes, 
                                                            beta = beta, gamma=gamma,
                                        alpha_fp =alpha_fp, 
                                        n_init = n_init, steps = steps, type_graph =graph_type,
                                        p=p, m=m,
                                        seed = int(seed * 1000 + exp),
                                        epsilon=0.001, do_plot = False,
                                        min_clip=0)
                        beta_v = np.array([beta] * n_nodes)
                        gamma_v = np.array([gamma] * n_nodes)
                        ground_truth = scenario['epidemic']['true_p']
                        for K in [10, 20]:
                            if (K == 10):
                                oracle_solution = np.zeros((K, n_nodes))
                                naive_solution = np.zeros((K, n_nodes))
                                solution = np.zeros((K, n_nodes))
                            else:
                                ground_truth = oracle_solution[-1,:]
                                oracle_solution = np.vstack([oracle_solution,
                                                           np.zeros((K-(oracle_solution).shape[0], n_nodes))])
                                naive_solution = np.vstack([naive_solution,
                                                           np.zeros((K-(naive_solution).shape[0], n_nodes))])
                                solution = np.vstack([solution,
                                                           np.zeros((K-(solution).shape[0], n_nodes))])
                                
                            range_K = np.arange(K)
                            if K == 20:
                                range_K = np.arange(10, K)
                            if K == 30:
                                range_K = np.arange(20, K)

                            for k in range_K:
                                ### Propagate solution
                                if (k > 0):
                                    oracle_solution[k,:] = np.clip(propagate_one_step_sparse(scenario['W'], ground_truth, beta_v, gamma_v), 0, 1)
                                    ground_truth = oracle_solution[k,:]
                                    naive_solution[k,:] = np.random.binomial(n=1, p=np.clip(ground_truth, 0, 1))
                                else:
                                    oracle_solution[k,:] = scenario['epidemic']['true_p']
                                    ground_truth = oracle_solution[k,:]
                                    naive_solution[k,:] =  scenario['epidemic']['y_observed']
                                
                                print('Infected: ' + str(naive_solution[k,:].sum()))
                                #### Add the Cross-validation procedure
                                print("k = " + str(k))
                                
                                results = np.zeros((len(lambda_range), nb_folds))
                                y_folds = np.zeros((n_nodes,nb_folds))
                                for fold in np.arange(nb_folds):
                                    y_folds[:, fold] = np.random.binomial(n=1, p=naive_solution[k,:]/nb_folds)

                                ##### Run k-fold crosss validation
                                for fold in np.arange(nb_folds):
                                    #fold = folds[j]
                                    #y_interpolated = copy.deepcopy(scenario['epidemic']['y_observed'])
                                    #y_interpolated[fold] = 0
                                    #y_test =  scenario['epidemic']['y_observed'][fold]
                                    y_test = y_folds[:, fold]
                                    y_interpolated = ( np.sum(y_folds, 1) - y_folds[:, fold]) / (nb_folds-1)
                                    for kk,lambda_ in enumerate(lambda_range):
                                        ssnal = SSNAL(gamma=lambda_, verbose=0)
                                        res_ssnal = ssnal.fit(X=y_interpolated.reshape((n_nodes,1)),
                                            weight_matrix=scenario['W'].todense(), save_centers=True)
                                        sol = res_ssnal.centers_.T
                                        sol = np.clip(sol, 0, 1).flatten()
                                        results[kk, fold] = np.mean(np.square(y_test - sol))
                                results_agg  = np.mean(results, 1)
                                index_lambda_best = np.argmin(results_agg)
                                lambda_best = lambda_range[index_lambda_best]
                                ssnal = SSNAL(gamma=lambda_best, verbose=0)
                                res_ssnal = ssnal.fit(X=naive_solution[k,:].reshape((n_nodes,1)),
                                            weight_matrix=scenario['W'].todense(), save_centers=True)
                                sol_temp = res_ssnal.centers_.T
                                sol_temp = np.clip(sol_temp, 0, 1).flatten()
                                #sol[np.where(scenario['epidemic']['y_observed']  == 1)[0]] = 
                                solution[k,:] =  sol_temp

                                print([k, np.sum(np.abs(solution[k,:].reshape((n_nodes,1)) - oracle_solution[k,:].reshape((n_nodes,1)))),
                                                np.sum(np.abs(naive_solution[k,:].reshape((n_nodes,1)) - oracle_solution[k,:].reshape((n_nodes,1))))])
                                
                                
                    
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

                            #naive_solution  = scenario['epidemic']['realized_state'][:,(steps):(steps + K)].T
                            Delta_p = np.zeros((K-1, 1))
                            Phi = np.zeros((K-1, 2))
                            #naive_solution  = scenario['epidemic']['realized_state'][:,(steps):(steps + K)].T
                            for k in np.arange(K-1):
                                Delta_p[k,:] =  np.sum(naive_solution[k+1,:] - naive_solution[k,:])
                                Phi[k,0] = np.sum(np.diag(np.array([1] * n_nodes) - naive_solution[k,:]).dot(scenario['W'] .dot(naive_solution[k,:])))
                                Phi[k,1] = -np.sum(naive_solution[k,:])
                            estimated_params_naive = np.linalg.pinv(Phi).dot(Delta_p)
                            
                            error_beta_naive= estimated_params_naive[0] - beta
                            error_gamma_naive = estimated_params_naive[1] - gamma


                            #oracle_solution  = scenario['epidemic']['track_state'][:,(steps):(steps + K)].T
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
                            print([exp, m, beta, gamma, steps,K,
                                                error_beta_ours[0], error_gamma_ours[0],
                                                error_beta_naive[0], error_gamma_naive[0],
                                                error_beta_oracle[0], error_gamma_oracle[0],
                                                estimated_params[0][0], estimated_params[1][0],
                                                estimated_params_naive[0][0], estimated_params_naive[1][0],
                                                estimated_params_oracle[0][0], estimated_params_oracle[1][0]
                                                ])
                            success.iloc[increment] = [exp, m, beta, gamma, steps,
                                                           K,
                                                np.sum(np.abs(solution[0,:].reshape((n_nodes,1)) - oracle_solution[0,:].reshape((n_nodes,1)))),
                                                np.sum(np.abs(naive_solution[0,:].reshape((n_nodes,1)) - oracle_solution[0,:].reshape((n_nodes,1)))),
                                                np.sum(np.abs(solution[K-1,:].reshape((n_nodes,1))- oracle_solution[K-1,:].reshape((n_nodes,1)))),
                                                np.sum(np.abs(naive_solution[K-1,:].reshape((n_nodes,1)) - oracle_solution[K-1,:].reshape((n_nodes,1)))),
                                                error_beta_ours[0], error_gamma_ours[0],
                                                error_beta_naive[0], error_gamma_naive[0],
                                                error_beta_oracle[0], error_gamma_oracle[0],
                                                estimated_params[0][0], estimated_params[1][0],
                                                estimated_params_naive[0][0], estimated_params_naive[1][0],
                                                estimated_params_oracle[0][0], estimated_params_oracle[1][0]
                                                ]
                            increment += 1
                            success.to_csv('~/Documents/epidemic_modelling/python/experiments/results/param' +  args.namefile + '.csv', index=False)



