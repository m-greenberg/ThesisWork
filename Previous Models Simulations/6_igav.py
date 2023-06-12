import numpy as np
import pandas as pd
from pathlib import Path

N = 5000
T = 100
steps = 100000

lams = [0.05, 0.33, 0.67, 0.95]
biases = [0.95, 0.9, 0.85, 0.8]
data = np.zeros((len(lams)+1, N))

for k, l in enumerate(lams):
    print(l)
    wealth = np.full(N, T)
    for t in np.arange(steps):
        i, j = np.random.choice(N, size=2, replace=False)
        dw = (1 - l) * np.min((wealth[i], wealth[j]))
        p = np.random.uniform()
        if p > biases[k]:  # wealthier agent wins
            if wealth[i] <= wealth[j]:
                wealth[i] -= dw
                wealth[j] += dw
            else:
                wealth[i] += dw
                wealth[j] -= dw
        else:  # poorer agent wins
            if wealth[i] <= wealth[j]:
                wealth[i] += dw
                wealth[j] -= dw
            else:
                wealth[i] -= dw
                wealth[j] += dw
    data[k] = wealth

print('q')
wealth = np.full(N, T)
qlams = np.linspace(0, 1, num=N)
for t in np.arange(steps):
    i, j = np.random.choice(N, size=2, replace=False)
    dw = np.min(((1 - qlams[i]) * wealth[i], (1 - qlams[j]) * wealth[j]))
    w_i = np.max((wealth[i], wealth[j]))
    w_j = np.min((wealth[i], wealth[j]))
    if w_i > 0:
        p = np.random.uniform()
        if p >= 0.5 + 0.5 * (w_i - w_j) / (w_i + w_j):  # wealthier agent wins
            if wealth[i] <= wealth[j]:
                wealth[i] -= dw
                wealth[j] += dw
            else:
                wealth[i] += dw
                wealth[j] -= dw
        else:  # poorer agent wins
            if wealth[i] <= wealth[j]:
                wealth[i] += dw
                wealth[j] -= dw
            else:
                wealth[i] -= dw
                wealth[j] += dw

data[4] = wealth
df = pd.DataFrame(data)
path = Path(f'C:/Users/maxgr/Desktop/visualizations/igav2.csv')
df.to_csv(path)
