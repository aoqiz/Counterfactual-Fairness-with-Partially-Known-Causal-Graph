import numpy as np
import networkx as nx
import matplotlib.pyplot as plt
import random

def set_random_seed(seed):
    random.seed(seed)
    np.random.seed(seed)

def show_dag(adjacency_matrix):
    rows, cols = np.where(adjacency_matrix == 1)
    edges = zip(rows.tolist(), cols.tolist())
    gr = nx.DiGraph()
    gr.add_edges_from(edges)
    nx.draw(gr, node_size=1200, arrows=True, with_labels=True)
    plt.show()