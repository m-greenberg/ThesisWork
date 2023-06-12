import numpy as np
import pandas as pd
from pathlib import Path

N = 5000
T = 100
steps = 100000

lambdas = [0, 0.2, 0.4, 0.6, 0.8, 0.95]
dist_lambdas = np.linspace(start=0, stop=1, num=N)

for lam in lambdas:
    print(lam)

    w = np.full(N, T)

    for t in np.arange(steps):
        i, j = np.random.choice(N, size=2, replace=False)
        eps = np.random.uniform()
        w_i = w[i]
        w_j = w[j]
        w[i] = (lam + eps * (1 - lam)) * w_i + eps * (1 - lam) * w_j
        w[j] = (1 - eps) * (1 - lam) * w_i + (lam + (1 - eps) * (1 - lam)) * w_j

    data = pd.DataFrame(w)
    id = str(lam).replace('.', '')
    path = Path(f'C:/Users/maxgr/Desktop/visualizations/cc{id}.csv')
    data.to_csv(path)

w = np.full(N, T)

for t in np.arange(steps):
    i, j = np.random.choice(N, size=2, replace=False)
    eps = np.random.uniform()
    w_i = w[i]
    w_j = w[j]
    w[i] = (dist_lambdas[i] + eps * (1 - dist_lambdas[i])) * w_i + eps * (1 - dist_lambdas[j]) * w_j
    w[j] = (1 - eps) * (1 - dist_lambdas[i]) * w_i + (dist_lambdas[j] + (1 - eps) * (1 - dist_lambdas[j])) * w_j

data = pd.DataFrame(w)
path = Path(f'C:/Users/maxgr/Desktop/visualizations/ccm.csv')
data.to_csv(path)
