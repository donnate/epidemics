import numpy as np
import pandas as pd
import time 
import numpy as np
import networkx as nx
import sys, os
sys.path.append('/Users/cdonnat/Documents/epidemic_modelling/python')
from simulate_epidemics import * 
from solvers import *
#from simulate_epidemics import * 
from solvers import *
from graph_utils import *

n = 1000 # number of nodes
p = 30 # vocab size
K = 20 # number of topics
r = 0.05 # heterogeneity parameter
m = 5 # number of neighbors to be considered in weights
phi = 0.1 #weight parameter
lambdas_ = [0.001, 0.1, 1, 10]
df = generate_graph(n)
weights = generate_weights(df, m, phi)
G, dfs_tree, path = generate_dfs(df, weights, n)
scenario = generate_scenario_with_graph(G, 
                                        beta = 0.99, gamma =0.0001,
            alpha_fp =0, 
            n_init = 1, steps = 20,
            epsilon=0.001, do_plot = False, min_clip=1e-5,
            seed = 1)

estimator_variancce = [None] * len(lambdas_)
for lambda_ in lambdas_:
    res_ssnal = ssnal_solver(scenario['epidemic']['y_observed'], scenario['W'], 6.)
    path = dfs_tree.edges()
    v= 0 
    it = 0
    for i, p in enumerate(path):
        if i%2 == 0:
            u = p[0]
            v = p[1]
            v += (2 * np.arcsin(np.sqrt(res_ssnal[u])) - 2 * np.arcsin(np.sqrt(res_ssnal[v])))**2
            it += 1
    print(v/it)
