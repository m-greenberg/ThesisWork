import networkx as nx
import numpy as np
import pandas as pd
from pathlib import Path

N = 5000
T = 10000
mu = 100
q = [0.1, 0.33, 0.67, 0.9]
s02 = 1

for q0 in q:
    w = np.full(N, mu)
    G = nx.barabasi_albert_graph(N, 2)
    nb = [list(nx.neighbors(G, i)) for i in np.arange(N)]
    z = np.fromiter((len(nb[i]) for i in np.arange(N)), dtype=int)

    for t in np.arange(T):
        w_t = np.copy(w)
        A_t = np.random.normal(0, np.sqrt(s02), N)
        I_t = np.fromiter((sum((q0/z[j]) * w_t[j] for j in nb[i]) for i in np.arange(N)), dtype=float)
        X_t = np.fromiter((q0 * w_t[i] for i in np.arange(N)), dtype=float)
        w = w_t + A_t + I_t - X_t

    id = str(q0).replace('.', '')
    data = pd.DataFrame(np.stack((w, z)))
    path = Path(f'C:/Users/maxgr/Desktop/visualizations/dm{id}.csv')
    data.to_csv(path)
