import argparse
import copy
import numpy as np
import pandas as pd
import time 
import numpy as np
import networkx as nx
import sys, os
#sys.path.append('/scratch/midway3/cdonnat/epidemics/python')
sys.path.append('/Users/cdonnat/Documents/epidemic_modelling/python')
from simulate_epidemics import * 
from solvers import *



#df = pd.read_table("/scratch/midway3/cdonnat/epidemics/data/socfb-Berkeley13.txt", sep=" ", header = None)
df = pd.read_table("/Users/cdonnat/Documents/epidemic_modelling/data/socfb-Berkeley13.csv", sep=" ", header = None)
df.columns = ["From", "To"]
G = nx.from_pandas_edgelist(df, source='From', target='To')
d_max = np.asarray(nx.degree(G))[:,1].max()

n_init = 1
columns = ['Experiment', 'Method', 'Time', 'Lambda', 'n_infected', 'p_er', 'Accuracy_true_p', 'Accuracy_true_y']
results_df = pd.DataFrame(columns=columns)


parser = argparse.ArgumentParser()
parser.add_argument('--namefile', type=str)
parser.add_argument('--seed', type=int)
parser.add_argument('--steps', type=int, default=20)
parser.add_argument('--n_step_predict', type=int, default=15)
parser.add_argument('--beta', type=float, default=0.9)
parser.add_argument('--gamma', type=float, default=0.1)
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
nb_folds=5

columns = ['Experiment', 'Method', 'Time',
            'Lambda', 'alpha_fp', 'p_sum', 
            'steps', 'steps_predict',
           'final_number_infected', 
           'Accuracy_true_p', 'Accuracy_true_y',
           'Accuracy_true_p_pos',
            'Accuracy_true_p_neg',  
           'bench_Accuracy_true_p', 'bench_Accuracy_true_y']
for step in np.arange(n_step_predict):
    columns += ['accuracy_prop_' + str(step), 
                'Accuracy_true_p_pos_'  + str(step),
                'Accuracy_true_p_neg_'  + str(step), 
               'accuracy_benchmark_prop_' + str(step)]
all_lambdas_ = [0.01, 0.05,  0.1, 0.5, 1, 2, 3, 5, 10, 15, 20, 30, 50, 80, 100, 200, 300, 500, 800, 1000]
results_df = pd.DataFrame(np.zeros(( 1000 * len(all_lambdas_),  len(columns))), columns=columns)
lambda_range = all_lambdas_

increment = 0
for exp in np.arange(1000):
        ### generate epidemic
    scenario = generate_scenario_with_graph(G, beta = beta, gamma =gamma,
                                            alpha_fp =alpha_fp, 
                                            n_init = 1, steps = steps,
                                            epsilon=0.001, do_plot = False, min_clip=1e-5,
                                            seed = args.seed)
    print('Infected: ' + str(scenario['epidemic']['y_observed'].sum()))
    for lambda_ in all_lambdas_:
        start_time = time.time() 
        res_ssnal = ssnal_solver(scenario['epidemic']['y_observed'],scenario['W'], lambda_)
        end_time = time.time() 
        res_ssnal = res_ssnal[:,0] 
        ### Propagate solution
        
        temp_res = [exp, 'SSNAL', end_time - start_time,  
                    lambda_,
            args.alpha_fp,
            scenario['epidemic']['y_true'].sum(),
            args.steps, args.n_step_predict,
            scenario['epidemic']['y_observed'].sum(),
            np.mean(np.abs(res_ssnal - scenario['epidemic']['true_p'])),
            np.mean(np.abs(res_ssnal  - scenario['epidemic']['y_true'])),
            np.mean(np.abs(res_ssnal - scenario['epidemic']['true_p'])[scenario['epidemic']['true_p'] > min_clip]),
            np.mean(np.abs(res_ssnal - scenario['epidemic']['true_p'])[scenario['epidemic']['true_p'] < min_clip]),
            np.mean(np.abs(scenario['epidemic']['y_observed'] - scenario['epidemic']['true_p'])),
            np.mean(np.abs(scenario['epidemic']['y_observed']  - scenario['epidemic']['y_true'])),
            ]
        current_p = res_ssnal
        current_p[np.where(current_p <min_clip)[0]] = 0
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
            current_p[np.where(current_p <min_clip)[0]] = 0
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
            current_p_observed[np.where(current_p_observed <min_clip)[0]] = 0
            if (ground_truth > 0).sum() <  0.8 * n_nodes:
                print("Using sparsity")
                ground_truth = propagate_one_step_sparse(scenario['W'], ground_truth, beta_v, gamma_v)
            else:
                ground_truth.resize((n_nodes,))
                ground_truth = propagate_one_step(scenario['W'], ground_truth, beta_v, gamma_v)
            ground_truth = np.reshape(ground_truth, (n_nodes,))
            ground_truth.resize((n_nodes,))
            ground_truth = np.asarray(ground_truth)
            ground_truth[np.where(ground_truth <min_clip)[0]] = 0
            temp_res += [np.mean(np.abs(current_p  - ground_truth)),
                            np.mean(np.abs(current_p - ground_truth)[ground_truth > min_clip]),
                            np.mean(np.abs(current_p - ground_truth)[ground_truth< min_clip]),
                            np.mean(np.abs(current_p_observed  - ground_truth))]
        results_df.loc[increment] = temp_res
        increment += 1
        res_ssnal[np.where(res_ssnal <min_clip)[0]] = 0
        results_df.to_csv('/Users/cdonnat/Documents/epidemic_modelling/python/' +  args.namefile + '.csv', index=False)
        #results_df.to_csv('/scratch/midway3/cdonnat/epidemics/python/experiments/results/results_semi_synthetic' +  args.namefile + '.csv', index=False)
        if np.mean(res_ssnal == 0) == 1:
            break


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
                lambda_best,
                args.alpha_fp,
                scenario['epidemic']['y_true'].sum(),
                args.steps, args.n_step_predict,
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
        current_p[np.where(current_p <min_clip)[0]] = 0
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
        current_p_observed[np.where(current_p_observed <min_clip)[0]] = 0
        if (ground_truth > 0).sum() <  0.8 * n_nodes:
            print("Using sparsity")
            ground_truth = propagate_one_step_sparse(scenario['W'], ground_truth, beta_v, gamma_v)
        else:
            ground_truth.resize((n_nodes,))
            ground_truth = propagate_one_step(scenario['W'], ground_truth, beta_v, gamma_v)
        ground_truth = np.reshape(ground_truth, (n_nodes,))
        ground_truth.resize((n_nodes,))
        ground_truth = np.asarray(ground_truth)
        ground_truth[np.where(ground_truth <min_clip)[0]] = 0
        temp_res += [np.mean(np.abs(current_p  - ground_truth)),
                        np.mean(np.abs(current_p - ground_truth)[ground_truth > min_clip]),
                        np.mean(np.abs(current_p - ground_truth)[ground_truth< min_clip]),
                        np.mean(np.abs(current_p_observed  - ground_truth))]


    results_df.loc[increment] = temp_res
    increment += 1


    #results_df.to_csv('/scratch/midway3/cdonnat/epidemics/python/experiments/results/results_semi_synthetic' +  args.namefile + '.csv', index=False)
    results_df.to_csv('/Users/cdonnat/Documents/epidemic_modelling/python/experiments/results/results_algo_semi_synthetic' +  args.namefile + '.csv', index=False)
                

