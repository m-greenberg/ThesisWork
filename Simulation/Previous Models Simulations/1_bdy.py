import numpy as np
import pandas as pd
from pathlib import Path

N = 5000
T = 100
steps = 100000

w = np.full(N, T)

for t in np.arange(steps):
    i, j = np.random.choice(N, size=2, replace=False)
    eps = np.random.uniform()
    delta_m = eps * (w[i] + w[j]) / 2
    if w[j] > delta_m:
        w[i] += delta_m
        w[j] -= delta_m

data = pd.DataFrame(w)
path = Path(f'C:/Users/maxgr/Desktop/visualizations/bdy.csv')
data.to_csv(path)
