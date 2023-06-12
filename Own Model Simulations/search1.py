from static_model import Model
from pathlib import Path
import numpy as np

# Estimated runtime: 1 hr 45 min

pc_lb = 0.05
pc_ub = 0.95
pc_step = 0.05

ps1_lb = 0.25
ps1_ub = 0.75
ps1_step = 0.05

eps = 0.0001


def main():

    M = Model()
    for pc in np.arange(pc_lb, pc_ub + eps, pc_step):
        for ps1 in np.arange(ps1_lb, ps1_ub + eps, ps1_step):
            pc = np.round(pc, 3)
            ps1 = np.round(ps1, 3)
            M.percent_cap = pc
            M.percent_sec_1 = ps1
            print(f"Current iteration: pc = {pc}, ps1 = {ps1}")
            M.simulate(pc, ps1)

    all_data = M.export_all_data()

    for label, data in all_data.items():
        path = Path(f'C:/Users/maxgr/Desktop/ThesisData/Search1/{label}.csv')
        data.to_csv(path)


if __name__ == "__main__":
    main()
