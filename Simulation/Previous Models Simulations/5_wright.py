import numpy as np
import pandas as pd
from pathlib import Path


class WrightModel:

    def __init__(self, N, M, w_a, w_b, Y=100):
        self.N = N
        self.M = M

        self.money = np.full(N, M/N)
        self.employers = np.full(N, -1)
        self.A = np.arange(N)

        self.V = 0
        self.w_a = w_a
        self.w_b = w_b
        self.mean_wage = (w_b - w_a)/2

        self.years = Y

        # DATA COLLECTION
        self.income = np.zeros(N)
        self.class_sizes = np.zeros((100, 3))

    def C(self):
        E = np.unique(self.employers)
        return np.delete(E, np.where(E == -1)[0])

    def W(self):
        return np.where(self.employers != -1)[0]

    def U(self):
        return np.delete(self.A, np.concatenate((self.C(), self.W())))

    def S1(self):
        return np.random.randint(low=0, high=self.N)

    def H1(self, a):
        if a in self.U():
            H = np.delete(self.A, np.concatenate(([a], self.W())))
            mH = np.sum(np.fromiter((self.money[x] for x in H), dtype=float))
            P = np.fromiter((self.money[c] / mH for c in H), dtype=float)
            c = np.random.choice(H, p=P)
            if self.money[c] > self.mean_wage:
                self.employers[a] = c

    def E1(self, a):
        b = np.random.choice(np.delete(np.copy(self.A), [a]))
        m = np.random.uniform(low=0, high=self.money[b])
        self.money[b] -= m
        self.V += m

    def M1(self, a):
        m = np.random.uniform(low=0, high=self.V)
        if a in self.W():
            e = self.employers[a]
            self.money[e] += m
            self.income[e] += m
            self.V -= m
        elif a in self.C():
            self.money[a] += m
            self.income[a] += m
            self.V -= m

    def F1(self, a):
        W_a = np.argwhere(np.isin(self.employers, [a])).ravel()
        u = np.max([len(W_a) - round(self.money[a]/self.mean_wage), 0])
        F = np.random.choice(W_a, size=u, replace=False)
        for f in F:
            self.employers[f] = -1

    def W1(self, a):
        W_a = np.argwhere(np.isin(self.employers, [a])).ravel()
        for c in W_a:
            w = np.random.uniform(low=self.w_a, high=self.w_b)
            if w > self.money[a]:
                w = np.random.uniform(low=0, high=self.money[a])
            self.money[c] += w
            self.income[c] += w
            self.money[a] -= w

    def SR1(self):
        a = self.S1()
        self.H1(a)
        self.E1(a)
        if a not in self.U():
            self.M1(a)
        self.F1(a)
        self.W1(a)

    def month_rule(self):
        for _ in np.arange(self.N):
            self.SR1()

    def year_rule(self):
        self.income = np.zeros(self.N)
        for _ in np.arange(12):
            self.month_rule()

    def run(self):
        for y in np.arange(self.years):
            print(f'Year {y+1}')
            self.year_rule()
            self.class_sizes[y] = [len(self.C()), len(self.W()), len(self.U())]


WM = WrightModel(N=5*10**3, M=5*10**5, w_a=10, w_b=90)
WM.run()

pop_data = pd.DataFrame(np.stack((WM.money, WM.income)))
size_data = pd.DataFrame(WM.class_sizes)

path1 = Path('C:/Users/maxgr/Desktop/visualizations/wright_pop.csv')
path2 = Path('C:/Users/maxgr/Desktop/visualizations/wright_size.csv')

pop_data.to_csv(path1)
size_data.to_csv(path2)
