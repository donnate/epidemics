import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.cluster import KMeans
from sklearn.metrics.pairwise import rbf_kernel
import networkx as nx

def get_initial_centers(val, centers):
    quantiles = []
    for i in range(centers):
        quantiles.append(i * int(val.shape[0]/centers))
    return quantiles
    
def generate_graph(n, seed = 0):
    np.random.seed(seed)
    coords = np.zeros((n, 2))
    coords[:, 0] = np.random.uniform(0, 1, n)
    coords[:, 1] = np.random.uniform(0, 1, n)

    cluster_obj = KMeans(n_clusters=20, init=coords[get_initial_centers(coords, 20), :], n_init=1)
    grps = cluster_obj.fit_predict(coords)

    df = pd.DataFrame(coords, columns=['x','y'])
    df['grp'] = grps
    return df

def generate_weights(df, nearest_n, phi):
    K = rbf_kernel(df[['x','y']], gamma = phi)
    np.fill_diagonal(K, 0)
    weights = np.zeros_like(K)

    for i in range(K.shape[0]):
        top_indices = np.argpartition(K[i], -nearest_n)[-nearest_n:]
        weights[i, top_indices] = K[i, top_indices]
        
    weights = (weights+weights.T)/2  
    # Adj = csr_matrix(weights)
    return weights

def plot_scatter(df):
    unique_groups = df['grp'].unique()
    cmap = plt.get_cmap('Set3', len(unique_groups))
    colors = [cmap(i) for i in range(len(unique_groups))]
    
    for group, color in zip(unique_groups, colors):
        grp_data = df[df['grp'] == group]
        plt.scatter(grp_data['x'], grp_data['y'], label=group, color=color)

def get_dfs_path(G, src):
    dfs_tree = nx.dfs_tree(G, source=src)
    path = dfs_tree.edges()
    return dfs_tree, path

def generate_dfs(df, weights, n):
    G = generate_graph_from_weights(df, weights, n)
    src = np.random.choice(np.arange(n))
    dfs_tree, path = get_dfs_path(G, src)
    return G, dfs_tree, path, src

def get_parent_node(mst, path, srn, nodenum):
    neighs = list(mst[nodenum].keys())
    length_to_srn = [path[neigh][srn] for neigh in neighs]
    parent = neighs[np.argmin(length_to_srn)]
    return parent


def get_folds(mst, path, n, nb_folds=2,
              plot_tree=False, colours_code = {0:"black",
                                          1: "red",
                                          2: "blue",
                                          3: "green",
                                          4: "pink",
                                          5: "purple",
                                           6:"orange",
                                          7: "brown",
                                          8: "grey",
                                          9: "yellow"
                                         }):
    srn = np.random.choice(range(n),1)[0]
    print(f"Source node is {srn}")
    
    folds = {}
    node_colors = [np.nan] * n
    nb_folds = 3
    paths = dict(nx.shortest_path_length(mst_tree, source=srn))
    for i in np.arange(nb_folds):
        folds[i] = [key for key, value in paths.items() if value % nb_folds == i]
        for u in folds[i]:
            node_colors[u] = i
    if plot_tree:
        nx.draw_kamada_kawai(mst, node_color = node_colors, node_size=10)
    return srn, folds, node_colors

def interpolate(X, G, folds, foldnum):
    fold = folds[foldnum]
    newX = copy.deepcopy(X)
    for node in fold:
        neighbours = list(G.neighbors(node))
        neighbours = list(set(neighbours) - set(fold))
        newX[node,:] = np.mean(X[neighbours,:], axis=0)
    return newX

def generate_graph_from_weights(df, weights, n):
    G = nx.Graph()
    for node in range(n):
        x = df['x'].iloc[node]
        y = df['y'].iloc[node]
        G.add_node(node, pos=(x, y))
    
    for node1 in G.nodes:
        for node2 in G.nodes:
            if node1 < node2:
                #pos1 = G.nodes[node1]['pos']
                #pos2 = G.nodes[node2]['pos']
                w = weights[node1,node2]
                #dist = norm(np.array(pos1) - np.array(pos2))
                if w > 0:
                    G.add_edge(node1, node2, weight=w)
    return G


def get_colors(df):
    grps = list(set(df['grp']))
    colors = []
    color_palette = ['cyan','yellow','greenyellow','coral','plum', 'blue',
                    'cyan','yellow','greenyellow','coral','plum', 'blue',
                    'cyan','yellow','greenyellow','coral','plum', 'blue',
                    'cyan','yellow','greenyellow','coral','plum', 'blue']
    colormap = {value: color for value, color in zip(grps, color_palette[:len(grps)])}

    for value in df['grp']:
        colors.append(colormap[value])
    return colors

def plot_2d_tree(colors, G, mst):
    pos = nx.get_node_attributes(G, 'pos')
    nx.draw(G, pos, with_labels=False, node_size=10, node_color=colors, edge_color='gray', alpha=0.6)
    nx.draw(mst, pos, with_labels=False, node_size=10, node_color=colors, edge_color='r', alpha=1)
    plt.show()