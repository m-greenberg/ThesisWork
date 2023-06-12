from static_model import Model
from pathlib import Path
import numpy as np

# Estimated runtime: 6 hours 20 minutes

prod_lb = 1
prod_ub = 10
prod_step = 0.25

cap_int_lb = 0.05
cap_int_ub = 0.95
cap_int_step = 0.05

eps = 0.0001


def main():

    M = Model()
    for prod in np.arange(prod_lb, prod_ub + eps, prod_step):
        for cap_int in np.arange(cap_int_lb, cap_int_ub + eps, cap_int_step):
            prod = np.round(prod, 3)
            cap_int = np.round(cap_int, 3)
            M.A = prod
            M.a = cap_int
            print(f"Current iteration: A = {prod}, a = {cap_int}")
            M.simulate(prod, cap_int)

    all_data = M.export_all_data()

    for label, data in all_data.items():
        path = Path(f'C:/Users/maxgr/Desktop/ThesisData/Search2/{label}.csv')
        data.to_csv(path)


if __name__ == "__main__":
    main()
