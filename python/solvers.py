import cvxpy as cp
import numpy as np
from numpy import linalg as la

import sys, os
sys.path.append('/Users/cdonnat/Documents/pycvxcluster/src/pycvxcluster')
from pycvxcluster import SSNAL, ADMM


def ssnal_solver(y_observed, W, lambda_):
    n_nodes  = len(y_observed)
    ssnal = SSNAL(gamma=lambda_, verbose=0)
    res_ssnal = ssnal.fit(X=y_observed.reshape((n_nodes,1)),
          weight_matrix=W.todense(), save_centers=True)
    sol = res_ssnal.centers_.T
    sol = np.clip(sol, 0, 1)
    return(sol)


def CV_ssnal_solver(y_observed, W, G, lambda_range, nb_folds = 3):
    ##### Start by creating a tree
    mst = nx.minimum_spanning_tree(G)
    path = dict(nx.shortest_path_length(mst, source=srn))
    n = nx.number_of_nodes(G)
    folds = get_folds(mst, path, n, nb_folds=nb_folds,
              plot_tree=False)
    ##### Run k-fold crosss validation
    y_interpolated = np.zeros((n, 1))
    results = np.zeros((len(lambda_range), nb_folds))
    for k, j in enumerate(folds.keys()):
        fold = folds[j]
        y_interpolated = interpolate(y_observed, mst, folds, foldnum)
        y_test =  y_observed[fold]
        for kk,lambda_ in enumerate(lambda_range):
            ssnal = SSNAL(gamma=lambda_, verbose=0)
            res_ssnal = ssnal.fit(X=y_interpolated.reshape((n_nodes,1)),
                  weight_matrix=W.todense(), save_centers=True)
            sol = res_ssnal.centers_.T
            sol = np.clip(sol, 0, 1)
            err = np.sum(np.square(y_test-sol[fold]) / len(y_test))
            results[k, kk] = err
    results_agg  = np.mean(results, 0)
    index_lambda_best = np.argmin(results_agg)
    lambda_best = lambda_range[index_lambda_best]

    n_nodes  = len(y_observed)
    ssnal = SSNAL(gamma=lambda_best, verbose=0)
    res_ssnal = ssnal.fit(X=y_observed.reshape((n_nodes,1)),
          weight_matrix=W.todense(), save_centers=True)
    sol = res_ssnal.centers_.T
    sol = np.clip(sol, 0, 1)
    return(sol)


def admm_solver(y_observed, W, lambda_):
    n_nodes  = len(y_observed)
    admm = ADMM(gamma=lambda_, verbose=0)
    res_admm = admm.fit(X=y_observed.reshape((n_nodes,1)),
          weight_matrix=W.todense(), save_centers=True)
    sol = res_admm.centers_.T
    sol = np.clip(sol, 0, 1)
    return(sol)


def cvx_solver(y_observed, Gamma, lambda_, p_norm=1):
    n_nodes = len(y_observed)
    
    # Indices where y_observed is not zero
    subject_0 = np.where(y_observed != 0)[0]
    print(subject_0)
    
    # Define the variable
    p = cp.Variable(n_nodes)
    
    # Constraints
    constraints = [p >= 0, p <= 1]
    # Define the quadratic loss
    loss = cp.sum_squares(y_observed - p) / n_nodes
    
    # Define the L-1 norm term (assuming Gamma is a numpy array or similar)
    l1_norm = cp.norm(Gamma @ p, p_norm)
    
    # Define the objective
    objective = cp.Minimize(loss + lambda_ * l1_norm)
    
    # Formulate and solve the problem
    problem = cp.Problem(objective, constraints)
    problem.solve()
    
    # Get the optimal value of p
    p_opt = p.value
    return p_opt


def cvx_solver_missing(y_observed, index_observed, Gamma, lambda_, p_norm=1):
    n_nodes = Gamma.shape[1]
    print('n_nodes = ' + str(n_nodes))
    # Define the variable
    p = cp.Variable(n_nodes)
    
    # Constraints
    constraints = [p >= 0, p <= 1]
    # Define the quadratic loss
    loss = cp.sum_squares(y_observed[index_observed] - p[index_observed]) / n_nodes
    
    # Define the L-1 norm term (assuming Gamma is a numpy array or similar)
    l1_norm = cp.norm(Gamma @ p, p_norm)
    
    # Define the objective
    objective = cp.Minimize(loss + lambda_ * l1_norm)
    
    # Formulate and solve the problem
    problem = cp.Problem(objective, constraints)
    problem.solve()
    
    # Get the optimal value of p
    p_opt = p.value
    return p_opt



def cgd_solver(y, Gamma, lambda_, eps = 1e-4, max_it = 50000):
    m, p = Gamma.shape
    Q = Gamma @ Gamma.T
    b = Gamma @ y

    u = np.zeros(m)
    n_iter = 0
    prev_u = 0 # For stopping criteria
    while True:
        n_iter += 1
        if n_iter >= max_it:
            #raise ValueError("Iterations exceed max_it")
            print("Iterations exceed max_it")
            return (y - Gamma.T @ u)
        for i in range(m):
            if Q[i, i] > 1e-4:
                t = 1/Q[i,i] * (b[i] - np.dot(np.delete(Q[i], i), np.delete(u, i)))
            else:
                t = 0

            u[i] = np.sign(t) * min(np.abs(t), lambda_)   #there should be better truncation methods

        if la.norm(u - prev_u) <= eps:
            break

        prev_u = np.copy(u)   # Recall array is similar to list

    beta = (y - Gamma.T @ u)
    return beta