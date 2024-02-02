steps = 10
n_step_predict = 10
n_init = 1
n_nodes = 1000
graph_type = "ER"
p = 0.01
m = 3
beta = 0.99
gamma = 0.001
min_clip = 1e-5
alpha_fp = 0


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
    print(it)
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
    print(step)
    ground_truth.resize((n_nodes,))
    ground_truth = np.asarray(ground_truth)
    ground_truth[np.where(ground_truth <min_clip)[0]] = 0
    temp_res += [np.mean(np.abs(current_p  - ground_truth)),
                    np.mean(np.abs(current_p_observed  - ground_truth))]