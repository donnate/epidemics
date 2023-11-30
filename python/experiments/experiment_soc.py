import argparse
import numpy as np
import pandas as pd
import time 
import numpy as np
import networkx as nx
import sys, os
sys.path.append('/scratch/midway3/cdonnat/epidemics/python')

from simulate_epidemics import * 
from solvers import *



df = pd.read_table("/scratch/midway3/cdonnat/epidemics/data/socfb-Berkeley13.txt", sep=" ", header = None)

n_init = 1
columns = ['Experiment', 'Method', 'Time', 'Lambda', 'n', 'p_er', 'Accuracy_true_p', 'Accuracy_true_y']
results_df = pd.DataFrame(columns=columns)


parser = argparse.ArgumentParser()
parser.add_argument('--namefile', type=str)
parser.add_argument('--seed', type=int)
parser.add_argument('--steps', type=int, default=40)
parser.add_argument('--n_step_predict', type=int, default=15)
parser.add_argument('--beta', type=float, default=0.99)
parser.add_argument('--gamma', type=float, default=0.001)
parser.add_argument('--alpha_fp', type=float, default=0.00)
parser.add_argument('--min_clip', type=float, default=1e-4)
args = parser.parse_args()

steps = args.steps
n_step_predict = args.n_step_predict
n_init = 1
beta = args.beta
gamma = args.gamma
min_clip = args.min_clip
alpha_fp = args.alpha_fp

columns = ['Experiment', 'Method', 'Time', 'Lambda', 'final_number_infected', 
           'Accuracy_true_p', 'Accuracy_true_y',
           'bench_Accuracy_true_p', 'bench_Accuracy_true_y']
for step in np.arange(n_step_predict):
    columns += ['accuracy_prop_' + str(step), 
               'accuracy_benchmark_prop_' + str(step)]
results_df = pd.DataFrame(columns=columns)

i = 0
for exp in np.arange(100):
            ### generate epidemic
            scenario = generate_scenario_with_graph(G, 
                                                    beta = beta, gamma =gamma,
                        alpha_fp =alpha_fp, 
                        n_init = 1, steps = steps,
                        epsilon=0.001, do_plot = False, min_clip=1e-5,
                        seed = args.seed)
            print('Infected: ' + str(scenario['epidemic']['y_observed'].sum()))
            for lambda_ in [0.01, 0.05,  0.1, 0.5, 1, 2, 3, 5, 10, 15, 20, 30, 50, 80, 100]:
                start_time = time.time() 
                res_ssnal = ssnal_solver(scenario['epidemic']['y_observed'],scenario['W'], lambda_)
                end_time = time.time() 
                res_ssnal = res_ssnal[:,0] 
                ### Propagate solution
                
                temp_res = [exp, 'SSNAL', end_time - start_time,  lambda_, scenario['epidemic']['y_observed'].sum(),
                                    np.mean(np.abs(res_ssnal - scenario['epidemic']['true_p'])),
                                    np.mean(np.abs(res_ssnal  - scenario['epidemic']['y_true'])),
                                    np.mean(np.abs(scenario['epidemic']['y_observed'] - scenario['epidemic']['true_p'])),
                                    np.mean(np.abs(scenario['epidemic']['y_observed']  - scenario['epidemic']['y_true'])),
                                    ]
                current_p = res_ssnal
                current_p[np.where(current_p <min_clip)[0]] = 0
                current_p_observed = scenario['epidemic']['y_observed']
                ground_truth  = scenario['epidemic']['true_p']
                n_nodes = nx.number_of_nodes(G)
                beta_v = [scenario['beta']] * n_nodes
                gamma_v = [scenario['gamma']] * n_nodes
                for it in np.arange(n_step_predict):
                    print(it)
                    if (current_p > 0).sum() <  0.8 * n_nodes:
                        print("Using sparsity")
                        current_p = propagate_one_step_sparse(scenario['W'], current_p, beta_v, gamma_v)
                    else:
                        current_p.resize((n_nodes,))
                        current_p = propagate_one_step(scenario['W'], current_p, beta_v, gamma_v)
                    current_p[np.where(current_p <min_clip)[0]] = 0
                    if (current_p_observed > 0).sum() <  0.8 * n_nodes:
                        print("Using sparsity")
                        current_p_observed = propagate_one_step_sparse(scenario['W'], current_p_observed, beta_v, gamma_v)
                    else:
                        current_p_observed.resize((n_nodes,))
                        current_p_observed = propagate_one_step(scenario['W'], current_p_observed, beta_v, gamma_v)
                    current_p_observed[np.where(current_p_observed <min_clip)[0]] = 0
                    if (ground_truth > 0).sum() <  0.8 * n_nodes:
                        print("Using sparsity")
                        ground_truth = propagate_one_step_sparse(scenario['W'], ground_truth, beta_v, gamma_v)
                    else:
                        ground_truth.resize((n_nodes,))
                        ground_truth = propagate_one_step(scenario['W'], ground_truth, beta_v, gamma_v)
                    ground_truth[np.where(ground_truth <min_clip)[0]] = 0
                    temp_res += [np.mean(np.abs(current_p  - ground_truth)),
                                 np.mean(np.abs(current_p_observed  - ground_truth))]

                

                results_df.loc[i] = temp_res
                i += 1


                results_df.to_csv('/scratch/midway3/cdonnat/epidemics/python/experiments/results/results_algo_semi_synthetic' +  args.namefile + '.csv', index=False)
                if np.mean(res_ssnal == 0) == 1:
                    break
                

