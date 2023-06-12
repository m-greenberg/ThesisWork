from static_model import Model
from pathlib import Path
import numpy as np

# Estimated runtime: 50 minutes

tax_lb = 0
tax_ub = 1
tax_step = 0.01

eps = 0.0001


def main():
    M = Model()
    for tax in np.arange(tax_lb, tax_ub + eps, tax_step):
        tax = np.round(tax, 3)
        M.wealth_tax = tax
        print(f"Current iteration: wtr = {tax}")
        M.simulate(tax)

    all_data = M.export_all_data()

    for label, data in all_data.items():
        path = Path(f'C:/Users/maxgr/Desktop/ThesisData/Search4/{label}.csv')
        data.to_csv(path)


if __name__ == "__main__":
    main()
