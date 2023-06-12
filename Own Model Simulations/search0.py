from static_model import Model
from pathlib import Path


def main():

    M = Model()
    M.simulate(0)

    all_data = M.export_all_data()

    for label, data in all_data.items():
        path = Path(f'C:/Users/maxgr/Desktop/ThesisData/Search0/{label}.csv')
        data.to_csv(path)


if __name__ == "__main__":
    main()
