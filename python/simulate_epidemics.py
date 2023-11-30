import matplotlib.pyplot as plt
import networkx as nx
import numpy as np
import scipy as sc
import time

def simulate_epidemic(W, y_init, beta_v, gamma_v,
                      steps = 1, 
                      alpha_fp = 0.1, seed = 1,
                      min_clip=0):
    """
    # W : weighted graph adjacency matrix
    # y_init : initial observations
    # beta :  infection probability
    # gamma : recovery probability
    """
    np.random.seed(seed)
    n_nodes = W.shape[1]

    #### Generate ground truth:
    y = y_init
    true_p = y_init
    track_state = np.zeros((n_nodes, steps + 1))
    track_state[:, 0] = y_init
    for step in np.arange(1,steps):
        print("Step "  + str(step) + " sparsity: " + str((true_p > 0).sum() ))
        if (true_p > 0).sum() <  0.8 * n_nodes:
            print("Using sparsity")
            true_p = propagate_one_step_sparse(W, true_p, beta_v, gamma_v)
        else:
            true_p.resize((n_nodes,))
            true_p = propagate_one_step(W, true_p, beta_v, gamma_v)
        print(true_p.shape)
        track_state[:, step] = true_p
        true_p = np.reshape(true_p, (n_nodes,))
        print(step)
        true_p.resize((n_nodes,))
        true_p = np.asarray(true_p)
        print(true_p.shape)
        true_p[np.where(true_p < min_clip)[0]] = 0
        #print(true_p)
    
    y_true = np.random.binomial(n=1, p=np.clip(true_p, 0, 1))
    
    it_p = 1
    while (y_true.sum() == 0) and  it_p < 100:
      ## resample to make sure someone is infectious (we must see someone infectious)
        y_true = np.random.binomial(n=1, p=np.clip(true_p, 0, 1)) 
        it_p = it_p + 1
        print("it_p: " + str(it_p))
        
    index_observed = np.where(y_true!=0)
    #### 
    y_false = np.random.binomial(n=1, p=[alpha_fp] * n_nodes)
    y_false[index_observed] = 0
    y_observed = np.clip(y_true + y_false, 0, 1)
    

    return({"true_p" : true_p,
            "y_observed" : y_observed,
            "beta_v": beta_v,
            "gamma_v": gamma_v,
            "y_false": y_false,
            "y_true": y_true,
            "track_state": track_state
            })    

def propagate_one_step_sparse(W, y, beta_v, gamma_v):
    """
    W : weighted graph adjacency matrix
    y_init : initial observations
    beta :  infection probability
    gamma : recovery probability
    """
    y_sp = sc.sparse.csc_matrix(y).T
    n_nodes = W.shape[0]
    D = sc.sparse.eye(n_nodes,format='csc')- sc.sparse.diags(gamma_v,format='csc') + \
        sc.sparse.diags(1 - y,format='csc') @ ( W @ sc.sparse.diags(beta_v,format='csc')) 
    true_p = D @ y_sp
    true_p = np.asarray(true_p.todense())[:,0]
    true_p = np.clip(true_p, 0, 1)
    return(true_p)


def propagate_one_step(W, y, beta_v, gamma_v):
    """
    W : weighted graph adjacency matrix
    y_init : initial observations
    beta :  infection probability
    gamma : recovery probability
    """
    W = W.todense()
    n_nodes = W.shape[0]
    D = np.eye(n_nodes)- np.diag(gamma_v) + \
        np.dot(np.diag(1 - y), np.dot( W, np.diag(beta_v)))
    true_p = np.dot(D, y.reshape((n_nodes, 1))).reshape((-1,))
    #print(D.shape, y.shape, true_p.shape)
    true_p = np.clip(true_p, 0, 1)
    return(true_p)

import time

def generate_scenario(n_nodes = 1000, beta = 0.9, gamma =0.1,
                      alpha_fp =0.001, 
                      n_init = 1, steps = 20, type_graph ="ER", p_er=0.01, 
                      p= 0.1, m=3,
                      seed = 1,
                      epsilon=0.01, do_plot = False,
                      min_clip=0):
    if type_graph == "ER":
        G = nx.erdos_renyi_graph(n=int(n_nodes), p=p_er, seed=seed)
    elif type_graph == "2Dgrid":
        G = nx.grid_2d_graph(n=int(np.sqrt(n_nodes)), m=int(np.sqrt(n_nodes)))
    elif type_graph == "expander":
        G = nx.paley_graph(p=n_nodes)
    elif type_graph == "small-world":
        G = nx.connected_watts_strogatz_graph(n_nodes, k=m, p=p, seed = seed)
    elif type_graph == "pa":
        G = nx.barabasi_albert_graph(n_nodes, m=m, seed =seed)
    elif type_graph == "power_law":
        G = nx.powerlaw_cluster_graph(n_nodes, m=m, p = p, seed =seed)
    else:
        print("Type of Graph Not implemented yet")
        return()
    n_nodes = nx.number_of_nodes(G)
    d_max = np.asarray(nx.degree(G))[:,1].max()
    weights = [None] * nx.number_of_edges(G)
    it = 0
    for (u, v) in G.edges():
        G[u][v]['weight'] = 1.0/(d_max + epsilon) 
        weights[it] = 1.0/(d_max + epsilon) #random.uniform(0, 10)  # Random weight between 0 and 10
        it += 1
    W = nx.adjacency_matrix(G)
    Gamma = 1.0/(d_max + epsilon) * nx.incidence_matrix(G, oriented=True)
    
    beta_v = np.array([beta] * n_nodes)
    gamma_v = np.array([gamma] * n_nodes)
    y_init = np.zeros(n_nodes)
    ind_init = int(np.ceil( np.random.random_sample(n_init) * n_nodes))
    y_init[ind_init] = 1
    if do_plot : 
        pos = nx.kamada_kawai_layout(G)
        plt.figure()
        nx.draw(G, pos = pos, node_color = y_init,
                edge_color='gray')
        plt.show()

    
    epidemic = simulate_epidemic(W, y_init, beta_v, gamma_v,
                steps = steps,
                alpha_fp = alpha_fp, seed=seed,
                min_clip=min_clip)
    if do_plot:
        for i in np.arange(steps + 1):
            plt.figure()
            nx.draw(G, pos = pos, node_color = epidemic["track_state"][:, i], edge_color='gray' )
            plt.show()
            time.sleep(1)
            
    return({"epidemic" : epidemic,
           "W" : W,
            "Gamma": Gamma,
            "y_init": y_init,
            "beta" : beta,
            "gamma" : gamma
           })


def generate_scenario_with_graph(G, beta = 0.9, gamma = 0.1,
                      alpha_fp =0.001, 
                      n_init = 1, steps = 20,
                      seed = 1,
                      epsilon=1e-6, do_plot = False,
                      min_clip=0):
    
    n_nodes = nx.number_of_nodes(G)
    d_max = np.asarray(nx.degree(G))[:,1].max()
    #d_max = np.asarray(nx.degree(G))[:,1].mean()
    weights = [None] * nx.number_of_edges(G)
    it = 0
    for (u, v) in G.edges():
        G[u][v]['weight'] = 1.0 /(d_max + epsilon) 
        weights[it] = 1.0 /(d_max + epsilon) #random.uniform(0, 10)  # Random weight between 0 and 10
        it += 1
    W = nx.adjacency_matrix(G)
    Gamma = 1.0/(d_max + epsilon) * nx.incidence_matrix(G, oriented=True)
    
    beta_v = np.array([beta] * n_nodes)
    gamma_v = np.array([gamma] * n_nodes)
    y_init = np.zeros(n_nodes)
    ##ind_init = int(np.ceil( np.random.random_sample(n_init) * n_nodes))
    t = np.argsort(np.array(nx.degree(G))[:,1], )[::-1][1:1000]
    np.random.shuffle(t)
    ind_init = t[0:n_init]
    y_init[ind_init] = 1
    if do_plot : 
        pos = nx.kamada_kawai_layout(G)
        plt.figure()
        nx.draw(G, pos = pos, node_color = y_init,
                edge_color='gray')
        plt.show()

    
    epidemic = simulate_epidemic(W, y_init, beta_v, gamma_v,
                steps = steps,
                alpha_fp = alpha_fp, seed=seed,
                min_clip=min_clip)
    if do_plot:
        for i in np.arange(steps + 1):
            plt.figure()
            nx.draw(G, pos = pos, node_color = epidemic["track_state"][:, i], edge_color='gray' )
            plt.show()
            time.sleep(1)
            
    return({"epidemic" : epidemic,
            "W" : W,
            "Gamma": Gamma,
            "y_init": y_init,
            "beta" : beta,
            "gamma" : gamma
           })
    