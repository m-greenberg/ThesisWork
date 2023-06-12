from static_model import Model
from pathlib import Path
import numpy as np

# Estimated runtime: 8 hours 20 minutes

w_lb = 1
w_ub = 10
w_step = 1

r_lb = 1
r_ub = 10
r_step = 1

p_lb = 1
p_ub = 10
p_step = 1

eps = 0.0001


def main():
    M = Model()
    for w in np.arange(w_lb, w_ub + eps, w_step):
        for r in np.arange(r_lb, r_ub + eps, r_step):
            for p in np.arange(p_lb, p_ub + eps, p_step):
                w = np.round(w, 3)
                r = np.round(r, 3)
                p = np.round(p, 3)
                M.w = w
                M.r = r
                M.p = p
                print(f"Current iteration: w = {w}, r = {r}, p = {p}")
                M.simulate(w, r, p)

    all_data = M.export_all_data()

    for label, data in all_data.items():
        path = Path(f'C:/Users/maxgr/Desktop/ThesisData/Search3/{label}.csv')
        data.to_csv(path)


if __name__ == "__main__":
    main()
