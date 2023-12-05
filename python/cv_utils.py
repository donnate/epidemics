import networkx as nx
import numpy as np

#### Implement cross validation on the network
#### Problem is homoskedasticity 
def get_mst_path(G):
    mst = nx.minimum_spanning_tree(G)
    path = dict(nx.all_pairs_shortest_path_length(mst))
    return mst, path

def generate_mst(df, weights, n):
    G = generate_graph_from_weights(df, weights, n)
    mst, path = get_mst_path(G)
    return G, mst, path

def get_parent_node(mst, path, srn, nodenum):
    neighs = list(mst[nodenum].keys())
    length_to_srn = [path[neigh][srn] for neigh in neighs]
    parent = neighs[np.argmin(length_to_srn)]
    return parent

def interpolate_X(X, folds, foldnum, path, mst, srn):
    fold = folds[foldnum]
    
    for node in fold:
        parent = get_parent_node(mst, path, srn, node)
        X[node,:] = X[parent,:]
    return X

def get_folds(mst, path, plot_tree=False, seed=2023):
    np.random.seed(seed)
    n = nx.number_of_nodes(mst)
    srn = np.random.choice(range(n),1)[0]
    srn = 0
    fold1 = []
    fold2 = []
    colors = [None] * n
    for key, value in path[srn].items():
        if (value%2)==0:
            fold1.append(key)
            colors[key]= "orange"
        elif (value%2)==1:
            fold2.append(key)
            colors[key]="blue"
        else:
            colors[key]="red"
    if plot_tree:
        nx.draw_kamada_kawai(mst, node_color = colors, node_size=10)
    return srn, fold1, fold2


def get_folds_p_sampling(G, n_folds, plot_tree=False, seed=2023):
    np.random.seed(seed)
    n = nx.number_of_nodes(G)
    sample_size = int(n/n_folds)
    for i in np.arange(n_folds):
        num_nodes = int(len(G.nodes()) * sample_size)
        sampled_nodes = random.sample(G.nodes(), num_nodes)
    for key, value in path[srn].items():
        if (value%2)==0:
            fold1.append(key)
            colors[key]= "orange"
        elif (value%2)==1:
            fold2.append(key)
            colors[key]="blue"
        else:
            colors[key]="red"
    if plot_tree:
        nx.draw_kamada_kawai(mst, node_color = colors, node_size=10)
    return srn, fold1, fold2