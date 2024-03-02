import argparse
import copy
import numpy as np
import pandas as pd
import time 
import numpy as np
import networkx as nx
import sys, os
sys.path.append('/scratch/midway3/cdonnat/epidemics_2/epidemics/python')

from simulate_epidemics import * 
from solvers import *


# Function to create disjoint neighborhoods
def create_disjoint_neighborhoods(G, Y):
    n_nodes = nx.number_of_nodes(G)
    nodes = set(np.arange(n_nodes))
    neighborhoods = []
    visited = set()
    stats = []
    while nodes:
        node = nodes.pop()
        if node in visited:
            continue
        # The neighborhood for this node includes itself and its neighbors
        neighborhood = list(nx.neighbors(G, n=node)) + [node]
        neighborhood = set(neighborhood) - set(visited)  ### remove the visited
        neighborhood = list(neighborhood)
        if len(neighborhood) ==1:
            #print("Damn")
            neighborhood = list(nx.neighbors(G, n=node)) + [node]
        ### compute statistics:
        stats += [np.mean(Y[list(neighborhood)])]
        neighborhoods.append(neighborhood)
        visited.update(neighborhood)
        nodes.difference_update(neighborhood) 
    return neighborhoods, stats
    

parser = argparse.ArgumentParser()
parser.add_argument('--namefile', type=str)
parser.add_argument('--seed', type=int)
parser.add_argument('--steps', type=int, default=20)
parser.add_argument('--n_step_predict', type=int, default=15)
parser.add_argument('--beta', type=float, default=0.9)
parser.add_argument('--gamma', type=float, default=0.1)
parser.add_argument('--alpha_fp', type=float, default=0.00)
parser.add_argument('--graph_type', type=str, default="knn")
parser.add_argument('--p', type=float, default=0.1)
parser.add_argument('--m', type=int, default=5)
parser.add_argument('--n_nodes', type=int, default=1000)
parser.add_argument('--min_clip', type=float, default=0)
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
min_clip = 0
min_clip_pred = 0.05
#min_clip_pred = 0.0
alpha_fp = args.alpha_fp
nb_folds = 2 
lambda_range = np.exp((np.arange(-4, 2, step=0.2)) * np.log(10))

#m_range = [2, 5, 7, 10]
#m_range = [1, 2, 5]
#m_range = [2, 10]
m_range = [5]
p_range = [0.1] #SW
p = 0.1
#p_range = [0.01, 0.005] #ER
#p_range = [0.05, 0.01] #SBM
#step_range = [80]
step_range = [20]



def BinaryCrossEntropy(y_true, y_pred):
    y_pred = np.clip(y_pred, 1e-7, 1 - 1e-7)
    term_0 = (1-y_true) * np.log(1-y_pred + 1e-7)
    term_1 = y_true * np.log(y_pred + 1e-7)
    return -np.mean(term_0+term_1, axis=0)


columns = ['Experiment', 'Method', 'Time',
           'graph_type',
            'n_nodes', 'nb_folds', 'cv_type',
            'alpha_fp',
            'gamma', 'beta',
            'p',  'm', 'n_infected', 'w',
            'row_max_W',
            'steps',
            'n_step_predict',
            'Lambda', 'final_number_infected', 
            'BCE',
            'Accuracy_true_p', 
             'Accuracy_true_p_l2', 
            'bench_BCE',
            'bench_Accuracy_true_p',
            'bench_Accuracy_true_p_l2']

print("columns")
print(len(columns))
for step in np.arange(n_step_predict):
    columns += ['BCE_' + str(step),
                'accuracy_prop_' + str(step), 
               'accuracy_prop_l2' + str(step),
                 'BCE_benchmark_'   + str(step),
               'accuracy_benchmark_prop_' + str(step),
'accuracy_prop_l2_bench' + str(step)]
    
print("columns")
print(len(columns))

results_df = pd.DataFrame(columns=columns)
print("dim results")
print(results_df.shape)
increment = 0
for exp in np.arange(500):
    for alpha_fp in [0]:
        for m in m_range:
            for cv_type in ["fission", "bootstrap"]:
                for nb_folds in [2, 5]:
                    for beta in [0.5]:
                        for steps in step_range:
                            ### generate epidemic
                            np.random.seed(1000 * args.seed + exp)
                            scenario = generate_scenario(n_nodes = n_nodes, 
                                                            beta = beta, gamma=gamma,
                                        alpha_fp =alpha_fp, 
                                        n_init = n_init, steps = steps, type_graph =graph_type,
                                        p=p, m=m,
                                        seed = 1000 * args.seed + exp,
                                        epsilon=0.001, do_plot = False,
                                        min_clip=min_clip)
                            print('Infected: ' + str(scenario['epidemic']['y_observed'].sum()))
                            #for lambda_ in [1e-4, 0.005, 0.001, 0.005, 0.01, 0.05,  0.1, 0.25, 0.5, 0.75, 1, 2, 3, 5, 10, 
                            #                    15, 20, 30, 50, 80, 100]:
                            for lambda_ in [1e-4]:
                                start_time = time.time() 
                                res_ssnal = ssnal_solver(scenario['epidemic']['y_observed'], scenario['W_binary'], lambda_)
                                end_time = time.time() 
                                res_ssnal = res_ssnal[:,0] 
                                #res_ssnal[np.where(scenario['epidemic']['y_observed']  == 1)[0]] = 1
                                
                                ### Propagate solution
                                
                                ['Experiment', 'Method', 'Time',
           'graph_type',
            'n_nodes', 'nb_folds', 'cv_type,'
            'alpha_fp',
            'gamma', 'beta',
            'p',  'm', 'n_infected', 'w',
            'row_max_W',
            'steps',
            'n_step_predict',
            'Lambda', 'final_number_infected', 
            'BCE',
            'Accuracy_true_p', 
             'Accuracy_true_p_l2', 
            'bench_BCE',
            'bench_Accuracy_true_p',
            'bench_Accuracy_true_p_l2']
                                temp_res =  [exp, 'SSNAL', end_time - start_time,  
                                            graph_type, n_nodes, nb_folds, cv_type,
                                            alpha_fp, args.gamma, beta,
                                            p, m, scenario['epidemic']['y_true'].sum(),
                                            scenario['W'].max(),
                                            scenario['W'].sum(1).max() * beta,
                                            steps, args.n_step_predict,
                                            lambda_, 
                                            scenario['epidemic']['y_observed'].sum(),
                                            BinaryCrossEntropy(scenario['epidemic']['true_p'], res_ssnal),
                                            np.sum(np.abs(res_ssnal - scenario['epidemic']['true_p'])),
                                            np.sum(np.square(res_ssnal - scenario['epidemic']['true_p'])),
                                            BinaryCrossEntropy(scenario['epidemic']['true_p'], scenario['epidemic']['y_observed'] ),
                                            np.sum(np.abs(scenario['epidemic']['y_observed'] - scenario['epidemic']['true_p'])),
                                            np.sum(np.square(scenario['epidemic']['y_observed'] - scenario['epidemic']['true_p']))
                                            ]
                                
                                print("Temp res 1")
                                print(temp_res)
                                print(len(temp_res))
                                current_p = res_ssnal
                                current_p[np.where(current_p <min_clip_pred)[0]] = 0
                                current_p_observed = scenario['epidemic']['y_observed']
                                ground_truth  = scenario['epidemic']['true_p']
                                n_nodes = scenario['W'].shape[0]
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
                                    current_p = np.reshape(current_p, (n_nodes,))
                                    print(step)
                                    current_p.resize((n_nodes,))
                                    current_p = np.asarray(current_p)
                                    current_p[np.where(current_p <min_clip_pred)[0]] = 0
                                    if (current_p_observed > 0).sum() <  0.8 * n_nodes:
                                        print("Using sparsity")
                                        current_p_observed = propagate_one_step_sparse(scenario['W'], current_p_observed, beta_v, gamma_v)
                                    else:
                                        current_p_observed.resize((n_nodes,))
                                        current_p_observed = propagate_one_step(scenario['W'], current_p_observed, beta_v, gamma_v)
                                    current_p_observed = np.reshape(current_p_observed, (n_nodes,))
                                    print(step)
                                    current_p_observed.resize((n_nodes,))
                                    current_p_observed = np.asarray(current_p_observed)
                                    current_p_observed[np.where(current_p_observed <min_clip_pred)[0]] = 0
                                    if (ground_truth > 0).sum() <  0.8 * n_nodes:
                                        print("Using sparsity")
                                        ground_truth = propagate_one_step_sparse(scenario['W'], ground_truth, beta_v, gamma_v)
                                    else:
                                        ground_truth.resize((n_nodes,))
                                        ground_truth = propagate_one_step(scenario['W'], ground_truth, beta_v, gamma_v)
                                    ground_truth = np.reshape(ground_truth, (n_nodes,))
                                    ground_truth.resize((n_nodes,))
                                    ground_truth = np.asarray(ground_truth)
                                    #ground_truth[np.where(ground_truth <min_clip)[0]] = 0
                                    temp_res += [BinaryCrossEntropy(ground_truth, current_p),
                                            np.sum(np.abs(current_p  - ground_truth)),
                                            np.sum(np.square(current_p  - ground_truth)),
                                            BinaryCrossEntropy(ground_truth, current_p_observed),
                                            np.sum(np.abs(current_p_observed  - ground_truth)),
                                            np.sum(np.square(current_p_observed  - ground_truth))]
                        
                                print("Len temp res" + str(len(temp_res)))
                                print("Len columns:" + str(len(columns)))
                                results_df.loc[increment] = temp_res
                                increment += 1
                                res_ssnal[np.where(res_ssnal <min_clip)[0]] = 0
                                if np.sum(res_ssnal == 0) == 1:
                                    break


                            #### Add the Cross-validation procedure

                            y_folds = np.zeros((n_nodes,nb_folds))
                            if cv_type == "fission":
                                for k in np.arange(nb_folds):
                                    print([cv_type, k])
                                    y_folds[:, k] = np.random.binomial(n=1, p=scenario['epidemic']['y_observed']/nb_folds)
                            else:
                                neighborhoods, stats = create_disjoint_neighborhoods(scenario['G'], scenario['epidemic']['y_observed'])
                                for k in np.arange(nb_folds):
                                    for kk in np.arange(len(neighborhoods)):
                                        indices = neighborhoods[kk]
                                        np.random.shuffle(stats)
                                        y_folds[indices, k] = np.random.binomial(n=1, p=stats[0], size=len(indices)) 

                            #path = dict(nx.shortest_path_length(mst, source=0))
                            #folds = get_folds(mst, path, n, nb_folds=nb_folds,
                            #          plot_tree=False)
                            ##### Run k-fold crosss validation
                            results = np.zeros((len(lambda_range), nb_folds))
                            results2 = np.zeros((len(lambda_range), nb_folds))
                            for k in np.arange(nb_folds):
                                #fold = folds[j]
                                #y_interpolated = copy.deepcopy(scenario['epidemic']['y_observed'])
                                #y_interpolated[fold] = 0
                                #y_test =  scenario['epidemic']['y_observed'][fold]
                                y_test = y_folds[:, k]
                                y_interpolated = ( np.sum(y_folds, 1) - y_folds[:, k]) / (nb_folds-1)
                                for kk,lambda_ in enumerate(lambda_range):
                                    ssnal = SSNAL(gamma=lambda_, verbose=0)
                                    res_ssnal = ssnal.fit(X=y_interpolated.reshape((n_nodes,1)),
                                        weight_matrix=scenario['W'].todense(), save_centers=True)
                                    sol = res_ssnal.centers_.T
                                    sol = np.clip(sol, 0, 1).flatten()
                                    results[kk, k] = BinaryCrossEntropy(y_test, sol)
                                    results2[kk, k] = np.mean(np.square(y_test - sol))
                            results_agg  = np.mean(results, 1)
                            results_agg2  = np.mean(results2, 1)
                            index_lambda_best = np.argmin(results_agg)
                            lambda_best = lambda_range[index_lambda_best]
                            index_lambda_best2 = np.argmin(results_agg2)
                            lambda_best2 = lambda_range[index_lambda_best2]
                            ssnal = SSNAL(gamma=lambda_best, verbose=0)
                            res_ssnal = ssnal.fit(X=scenario['epidemic']['y_observed'].reshape((n_nodes,1)),
                                        weight_matrix=scenario['W'].todense(), save_centers=True)
                            sol = res_ssnal.centers_.T
                            sol = np.clip(sol, 0, 1).flatten()
                            #sol[np.where(scenario['epidemic']['y_observed']  == 1)[0]] = 1
                            res_ssnal = sol
                            ### Propagate solution
                            print("here")
                            temp_res = [exp, 'SSNAL-opt', end_time - start_time,  
                                            graph_type, n_nodes, nb_folds, cv_type,
                                            alpha_fp, args.gamma, beta,
                                            p, m, scenario['epidemic']['y_true'].sum(),
                                            scenario['W'].max(),
                                            scenario['W'].sum(1).max() * beta,
                                            steps, args.n_step_predict,
                                            lambda_best, 
                                            scenario['epidemic']['y_observed'].sum(),
                                            BinaryCrossEntropy(scenario['epidemic']['true_p'], res_ssnal),
                                            np.sum(np.abs(res_ssnal - scenario['epidemic']['true_p'])),
                                            np.sum(np.square(res_ssnal - scenario['epidemic']['true_p'])),
                                            BinaryCrossEntropy(scenario['epidemic']['true_p'], scenario['epidemic']['y_observed'] ),
                                            np.sum(np.abs(scenario['epidemic']['y_observed'] - scenario['epidemic']['true_p'])),
                                            np.sum(np.square(scenario['epidemic']['y_observed'] - scenario['epidemic']['true_p']))
                                            ]
                            current_p = res_ssnal
                            current_p[np.where(current_p <min_clip_pred)[0]] = 0
                            current_p_observed = scenario['epidemic']['y_observed']
                            ground_truth  = scenario['epidemic']['true_p']
                            n_nodes = scenario['W'].shape[0]
                            beta_v = [scenario['beta']] * n_nodes
                            gamma_v = [scenario['gamma']] * n_nodes
                            for it in np.arange(n_step_predict):
                                #print(it)
                                if (current_p > 0).sum() <  0.8 * n_nodes:
                                    #print("Using sparsity")
                                    current_p = propagate_one_step_sparse(scenario['W'], current_p, beta_v, gamma_v)
                                else:
                                    current_p.resize((n_nodes,))
                                    current_p = propagate_one_step(scenario['W'], current_p, beta_v, gamma_v)
                                current_p = np.reshape(current_p, (n_nodes,))
                                #print(step)
                                current_p.resize((n_nodes,))
                                current_p = np.asarray(current_p)
                                current_p[np.where(current_p <min_clip_pred)[0]] = 0
                                if (current_p_observed > 0).sum() <  0.8 * n_nodes:
                                    #print("Using sparsity")
                                    current_p_observed = propagate_one_step_sparse(scenario['W'], current_p_observed, beta_v, gamma_v)
                                else:
                                    current_p_observed.resize((n_nodes,))
                                    current_p_observed = propagate_one_step(scenario['W'], current_p_observed, beta_v, gamma_v)
                                current_p_observed = np.reshape(current_p_observed, (n_nodes,))
                                #print(step)
                                current_p_observed.resize((n_nodes,))
                                current_p_observed = np.asarray(current_p_observed)
                                current_p_observed[np.where(current_p_observed <min_clip_pred)[0]] = 0
                                
                                if (ground_truth > 0).sum() <  0.8 * n_nodes:
                                    #print("Using sparsity")
                                    ground_truth = propagate_one_step_sparse(scenario['W'], ground_truth, beta_v, gamma_v)
                                else:
                                    ground_truth.resize((n_nodes,))
                                    ground_truth = propagate_one_step(scenario['W'], ground_truth, beta_v, gamma_v)
                                ground_truth = np.reshape(ground_truth, (n_nodes,))
                                ground_truth.resize((n_nodes,))
                                ground_truth = np.asarray(ground_truth)
                                #ground_truth[np.where(ground_truth <min_clip)[0]] = 0
                                temp_res += [BinaryCrossEntropy(ground_truth, current_p),
                                            np.sum(np.abs(current_p  - ground_truth)),
                                            np.sum(np.square(current_p  - ground_truth)),
                                            #np.sum(np.abs(current_p - ground_truth)[ground_truth > min_clip]),
                                            #np.sum(np.abs(current_p - ground_truth)[ground_truth< min_clip]),
                                            BinaryCrossEntropy(ground_truth, current_p_observed),
                                            np.sum(np.abs(current_p_observed  - ground_truth)),
                                            np.sum(np.square(current_p_observed  - ground_truth))]

                            print("Len temp res" + str(len(temp_res)))
                            print("Len columns:" + str(len(columns)))
                            results_df.loc[increment] = temp_res
                            increment += 1
                            


                            ssnal = SSNAL(gamma=lambda_best2, verbose=0)
                            res_ssnal = ssnal.fit(X=scenario['epidemic']['y_observed'].reshape((n_nodes,1)),
                                        weight_matrix=scenario['W'].todense(), save_centers=True)
                            sol = res_ssnal.centers_.T
                            sol = np.clip(sol, 0, 1).flatten()
                            #sol[np.where(scenario['epidemic']['y_observed']  == 1)[0]] = 1
                            res_ssnal = sol
                            ### Propagate solution

                            temp_res = [exp, 'SSNAL-opt2', end_time - start_time,  
                                            graph_type, n_nodes, nb_folds, cv_type,
                                            alpha_fp, args.gamma, beta,
                                            p, m, scenario['epidemic']['y_true'].sum(),
                                            scenario['W'].max(),
                                            scenario['W'].sum(1).max() * beta,
                                            steps, args.n_step_predict,
                                            lambda_best2, 
                                            scenario['epidemic']['y_observed'].sum(),
                                            BinaryCrossEntropy(scenario['epidemic']['true_p'], res_ssnal),
                                            np.sum(np.abs(res_ssnal - scenario['epidemic']['true_p'])),
                                            np.sum(np.square(res_ssnal - scenario['epidemic']['true_p'])),
                                            BinaryCrossEntropy(scenario['epidemic']['true_p'], scenario['epidemic']['y_observed'] ),
                                            np.sum(np.abs(scenario['epidemic']['y_observed'] - scenario['epidemic']['true_p'])),
                                            np.sum(np.square(scenario['epidemic']['y_observed'] - scenario['epidemic']['true_p']))
                                            ]
                            current_p = res_ssnal
                            current_p[np.where(current_p <min_clip)[0]] = 0
                            current_p_observed = scenario['epidemic']['y_observed']
                            ground_truth  = scenario['epidemic']['true_p']
                            n_nodes = scenario['W'].shape[0]
                            beta_v = [scenario['beta']] * n_nodes
                            gamma_v = [scenario['gamma']] * n_nodes
                            for it in np.arange(n_step_predict):
                                #print(it)
                                if (current_p > 0).sum() <  0.8 * n_nodes:
                                    #print("Using sparsity")
                                    current_p = propagate_one_step_sparse(scenario['W'], current_p, beta_v, gamma_v)
                                else:
                                    current_p.resize((n_nodes,))
                                    current_p = propagate_one_step(scenario['W'], current_p, beta_v, gamma_v)
                                current_p = np.reshape(current_p, (n_nodes,))
                                #print(step)
                                current_p.resize((n_nodes,))
                                current_p = np.asarray(current_p)
                                current_p[np.where(current_p <min_clip_pred)[0]] = 0
                                if (current_p_observed > 0).sum() <  0.8 * n_nodes:
                                    #print("Using sparsity")
                                    current_p_observed = propagate_one_step_sparse(scenario['W'], current_p_observed, beta_v, gamma_v)
                                else:
                                    current_p_observed.resize((n_nodes,))
                                    current_p_observed = propagate_one_step(scenario['W'], current_p_observed, beta_v, gamma_v)
                                current_p_observed = np.reshape(current_p_observed, (n_nodes,))
                                #print(step)
                                current_p_observed.resize((n_nodes,))
                                current_p_observed = np.asarray(current_p_observed)
                                current_p_observed[np.where(current_p_observed <min_clip_pred)[0]] = 0
                                if (ground_truth > 0).sum() <  0.8 * n_nodes:
                                    #print("Using sparsity")
                                    ground_truth = propagate_one_step_sparse(scenario['W'], ground_truth, beta_v, gamma_v)
                                else:
                                    ground_truth.resize((n_nodes,))
                                    ground_truth = propagate_one_step(scenario['W'], ground_truth, beta_v, gamma_v)
                                ground_truth = np.reshape(ground_truth, (n_nodes,))
                                ground_truth.resize((n_nodes,))
                                ground_truth = np.asarray(ground_truth)
                                #ground_truth[np.where(ground_truth <min_clip)[0]] = 0
                                temp_res += [BinaryCrossEntropy(ground_truth, current_p),
                                            np.sum(np.abs(current_p  - ground_truth)),
                                            np.sum(np.square(current_p  - ground_truth)),
                                            #np.sum(np.abs(current_p - ground_truth)[ground_truth > min_clip]),
                                            #np.sum(np.abs(current_p - ground_truth)[ground_truth< min_clip]),
                                            BinaryCrossEntropy(ground_truth, current_p_observed),
                                            np.sum(np.abs(current_p_observed  - ground_truth)),
                                            np.sum(np.square(current_p_observed  - ground_truth))]
                        

                            print("Len temp res" + str(len(temp_res)))
                            print("Len columns:" + str(len(columns)))
                            results_df.loc[increment] = temp_res
                            increment += 1



                            #results_df.to_csv('/scratch/midway3/cdonnat/epidemics_2/epidemics/python/experiments/results/new_CV' + str(nb_folds) + '_res' +  args.namefile + '.csv', index=False)
                            results_df.to_csv('~/Downloads/newest_' +   args.namefile  + '_res_' +  args.graph_type + '.csv', index=False)



                                

