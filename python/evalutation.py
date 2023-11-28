import numpy as np

from simulate_epidemics import propagate_one_step

def evaluate_solution(y_observed, p_hat, true_prob, Gamma):
    return({"risk_observed" = np.mean((y_observed - p_hat)**2),
            "oracle_risk" = np.mean((y_observed -  true_prob)**2),
            "l1_error" = np.mean(np.abs(true_prob - p_hat)),
            "Gamma_l1_error" = np.mean(np.abs(Gamma.dot(true_prob - p_hat))),
            "Gamma_l1_error_benchmark" = np.mean(np.abs(Gamma.dot(true_prob - y_observed)))})


def propagate_solution(W, p_hat, beta_v, gamma_v, nb_steps):
    p_current = p_hat
    n = length(p_current)
    list_p = np.zeros((nb_steps, n))
    for it in np.arange(nb_steps):
        prop_results = propagate_one_step(W, as.numeric(p_current), beta_v, gamma_v)
        p_current  = np.apply(prop_results["true_p"], lambda x: np.min(np.max(x,0), 1))
        list_p[it,:] = p_current
    return(list_p)

