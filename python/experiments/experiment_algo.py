import numpy as np
import pandas as pd
import time 
import numpy as np
import networkx as nx
import sys, os
sys.path.append('/Users/cdonnat/Documents/epidemic_modelling/python')

from simulate_epidemics import * 
from solvers import *

type_graph = "ER"
steps = 20
n_init = 1
columns = ['Experiment', 'Method', 'Time', 'Lambda', 'n', 'p_er', 'Accuracy_true_p', 'Accuracy_true_y']
results_df = pd.DataFrame(columns=columns)

i = 0
for exp in np.arange(100):
    for n_nodes in [200, 500, 1000, 5000, 10000]:
        for p_er in [0.005, 0.01, 0.02]:
            ### generate epidemic
            scenario = generate_scenario(n_nodes = n_nodes, beta = 0.9, gamma =0.1,
                        alpha_fp =0.00, 
                        n_init = n_init, steps = steps, type_graph ="ER", p_er=p_er, 
                        epsilon=0.01, do_plot = False)
            for lambda_ in [1e-4, 5 * 1e-4, 1e-3, 0.005, 0.01, 0.05, 0.1, 0.5, 1, 5, 10, 20, 30, 50, 80, 100, 200, 500, 1000]:
                start_time = time.time() 
                res_cvx = cvx_solver(scenario['epidemic']['y_observed'], scenario['Gamma'].todense().T, lambda_)
                end_time = time.time() 
                results_df.loc[i] = [exp, 'CVX', end_time - start_time,  lambda_, n_nodes, p_er, 
                                    np.mean(np.abs(res_cvx  - scenario['epidemic']['true_p'])),
                                    np.mean(np.abs(res_cvx  - scenario['epidemic']['y_true'])) ]
                i += 1

                start_time = time.time() 
                res_admm = admm_solver(scenario['epidemic']['y_observed'], scenario['W'], lambda_)
                end_time = time.time() 
                res_admm = res_admm[:,0]
                results_df.loc[i] = [exp, 'ADMM', end_time - start_time,  lambda_, n_nodes, p_er, 
                                    np.mean(np.abs(res_admm  - scenario['epidemic']['true_p'])),
                                    np.mean(np.abs(res_admm  - scenario['epidemic']['y_true'])) ]
                i += 1

                start_time = time.time() 
                res_ssnal = ssnal_solver(scenario['epidemic']['y_observed'],scenario['W'], lambda_)
                end_time = time.time() 
                res_ssnal = res_ssnal[:,0]
                results_df.loc[i] = [exp, 'SSNAL', end_time - start_time,  lambda_, n_nodes, p_er, 
                                    np.mean(np.abs(res_ssnal - scenario['epidemic']['true_p'])),
                                    np.mean(np.abs(res_ssnal  - scenario['epidemic']['y_true']))]
                i += 1

                start_time = time.time() 
                res_cgd = cgd_solver(scenario['epidemic']['y_observed'],scenario['Gamma'].todense().T, lambda_)
                end_time = time.time() 
                results_df.loc[i] = [exp, 'CGD', end_time - start_time,  lambda_, n_nodes, p_er, 
                                    np.mean(np.abs(res_cgd - scenario['epidemic']['true_p'])),
                                    np.mean(np.abs(res_cgd  - scenario['epidemic']['y_true']))]
                i += 1

                results_df.to_csv('~/Documents/epidemic_modelling/python/experiments/results_algo.csv', index=False)





