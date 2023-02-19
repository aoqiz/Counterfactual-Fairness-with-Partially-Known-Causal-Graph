import numpy as np
from DataLoader.synthetic_dataset import DAG_simulation, simulate_data
from DataLoader.utils import show_dag, set_random_seed
import pandas as pd
import os
import argparse
# ------Parameters setting------
#seed
set_random_seed(532)
parser = argparse.ArgumentParser()
parser.add_argument('num_of_nodes', type=int, help='the number of nodes')
parser.add_argument('num_of_edges', type=int, help='the number of edges')
args = parser.parse_args()
#number of nodes
d = args.num_of_nodes
#number of edges
s = args.num_of_edges

print("{} nodes {} edges: ".format(d, s))

# number of graphs
G = 100
# number of observations
n = 1000
# number of samples
m = 1
#discrete variable and classes, for examples dv_ls=[4,5] dv_c_ls=[2,3] means node 4 is of [0,1] and node 5 of [0,1,2]
#dv_c_ls = np.random.choice([2,3], size=1).tolist()
dv_c_ls = [2]
#noise setting
noise_type = 'gaussian'
v_noise = 1
# save target path
save_path="Repository/{}nodes{}edges".format(d, s)
os.makedirs(save_path, exist_ok=True)

for i in range(G):
    print(i)
    DAG = DAG_simulation(d, s, lb=0.5, ub=2.0)
    B_bin = DAG.B_bin
    B = DAG.B
    dv_ls, B, observational_data, counterfactual_data = simulate_data(dv_c_ls, B, n*m, noise_type, v_noise)
    outcome = np.random.choice(np.array(list(set(np.arange(0,d)).difference(dv_ls))), size=1)[0]
    # print('Sensitive attributes:', format(dv_ls))
    # print('Number of classes for each sensitive attribute:', format(dv_c_ls))
    # print('Outcome:', format(outcome))

    #save data
    pd.DataFrame(B_bin).to_csv("{}/adjacency_matrix_{}.csv".format(save_path, i), index=False, header=False)
    pd.DataFrame(B).to_csv("{}/weight_matrix_{}.csv".format(save_path, i), index=False, header=False)
    pd.DataFrame(observational_data).to_csv("{}/observational_data_{}.csv".format(save_path, i), index=False, header=False)
    pd.DataFrame(counterfactual_data).to_csv("{}/counterfactual_data_{}.csv".format(save_path, i), index=False, header=False)
    with open("{}/config_{}.txt".format(save_path, i), "w") as f:
        f.writelines('protected, protected_classes, outcome, num_sample, sample_size\n'+ str(dv_ls[0]+1) + ',' + str(dv_c_ls[0]) + ',' + str(outcome+1) + ',' + str(m) + ',' + str(n) + '\n')
