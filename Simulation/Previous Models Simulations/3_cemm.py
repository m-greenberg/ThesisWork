import numpy as np
import pandas as pd
from pathlib import Path

N = 5000
T = 100
steps = 10_000_000

w = np.random.uniform(size=N)

for t in np.arange(steps):
    p = np.argmin(w)
    w_prime = np.random.uniform()
    delta_w = w_prime - w[p]
    w[p] += delta_w
    if p == 0:
        w[p+1] -= delta_w
    elif p == N - 1:
        w[p-1] -= delta_w
    else:
        w[p-1] -= delta_w/2
        w[p+1] -= delta_w/2

data = pd.DataFrame(w)
path = Path(f'C:/Users/maxgr/Desktop/visualizations/cemm.csv')
data.to_csv(path)
