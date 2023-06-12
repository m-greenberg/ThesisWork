from math import ceil
import numpy as np
import pandas as pd


class Model:

    def __init__(self, n=5000, t=100, pc=0.2, ps1=0.5, ife=100.0, iw=100.0, max_j=np.infty, prod=8.0, cap_int=0.5,
                 wage=6.0, price=6.0, cap_price=4.0, wtr=0.0, itr=0.0, ctr=0.0, ptr=0.0, ttr=0.0):
        # SIMULATION VARIABLES
        self.N_pop = n
        self.steps = t

        # INITIALIZATION VARIABLES
        self.percent_cap = pc
        self.percent_sec_1 = ps1
        self.init_firm_endowment = ife
        self.init_wealth = iw
        self.max_jobs = max_j

        # PRODUCTION VARIABLES
        self.A = prod
        self.a = cap_int

        # TAX VARIABLES
        self.wealth_tax = wtr
        self.income_tax = itr
        self.sales_tax = ctr
        self.payroll_tax = ptr
        self.turnover_tax = ttr

        # PRICE VARIABLES
        self.wage = wage
        self.price = price
        self.cap_price = cap_price

        self.w = None
        self.p = None
        self.r = None

        # SIMULATION VARIABLE DEFINITION
        self.N_c = None
        self.N_w = None
        self.N_c1 = None
        self.N_c2 = None

        self.pop = None
        self.pop_indices = None
        self.cap_indices = None
        self.cap1_indices = None
        self.cap2_indices = None
        self.worker_indices = None

        self.pop_indices_copy = None
        self.cap_indices_copy = None
        self.cap1_indices_copy = None
        self.cap2_indices_copy = None
        self.worker_indices_copy = None

        self.firm_stock = None
        self.labor_supply = None
        self.supply = None

        # DATAFRAME DEFINITION
        self.wealth_dist = None
        self.income_dist = None

        self.wealth_gini_ts = None
        self.income_gini_ts = None
        self.mean_wealth_cap1_ts = None
        self.mean_wealth_cap2_ts = None
        self.mean_wealth_workers_ts = None
        self.mean_income_cap1_ts = None
        self.mean_income_cap2_ts = None
        self.mean_income_workers_ts = None
        self.mean_wealth_global_ts = None
        self.mean_income_global_ts = None
        self.deciles_wealth_cap1 = None
        self.deciles_wealth_cap2 = None
        self.deciles_wealth_workers = None
        self.deciles_income_cap1 = None
        self.deciles_income_cap2 = None
        self.deciles_income_workers = None
        self.deciles_wealth_global = None
        self.deciles_income_global = None
        self.k_expenditures_ts = None
        self.l_expenditures_ts = None
        self.c_expenditures_ts = None
        self.unemployment_ts = None

        self.all_wealth_dists = []
        self.all_income_dists = []

        self.all_deciles_wealth_cap1 = []
        self.all_deciles_wealth_cap2 = []
        self.all_deciles_wealth_workers = []
        self.all_deciles_income_cap1 = []
        self.all_deciles_income_cap2 = []
        self.all_deciles_income_workers = []
        self.all_deciles_wealth_global = []
        self.all_deciles_income_global = []

        self.all_wealth_ginis = []
        self.all_income_ginis = []
        self.all_mean_wealth_cap1 = []
        self.all_mean_wealth_cap2 = []
        self.all_mean_wealth_workers = []
        self.all_mean_income_cap1 = []
        self.all_mean_income_cap2 = []
        self.all_mean_income_workers = []
        self.all_mean_wealth_global = []
        self.all_mean_income_global = []
        self.all_k_expenditures = []
        self.all_l_expenditures = []
        self.all_c_expenditures = []
        self.all_unemployment_rates = []

        self.parameters = []

        self.pop_data = [self.wealth_dist, self.income_dist]
        self.all_pop_data = [self.all_wealth_dists, self.all_income_dists]
        self.all_pop_data_titles = ["final_wealth_dist", "final_income_dist"]

        self.decile_data = None
        self.all_decile_data = [self.all_deciles_wealth_cap1, self.all_deciles_wealth_cap2, self.all_deciles_wealth_workers,
                                self.all_deciles_income_cap1, self.all_deciles_income_cap2, self.all_deciles_income_workers,
                                self.all_deciles_wealth_global, self.all_deciles_income_global]
        self.all_decile_data_titles = ["wealth_deciles_s1", "wealth_deciles_s2", "wealth_deciles_w",
                                       "income_deciles_s1", "income_deciles_s2", "income_deciles_w",
                                       "wealth_deciles", "income_deciles"]

        self.ts_data = None
        self.all_ts_data = [self.all_wealth_ginis, self.all_income_ginis,
                            self.all_mean_wealth_cap1, self.all_mean_wealth_cap2, self.all_mean_wealth_workers,
                            self.all_mean_income_cap1, self.all_mean_income_cap2, self.all_mean_income_workers,
                            self.all_mean_wealth_global, self.all_mean_income_global,
                            self.all_k_expenditures, self.all_l_expenditures, self.all_c_expenditures,
                            self.all_unemployment_rates]
        self.all_ts_data_titles = ["wealth_gini", "income_gini",
                                   "mean_wealth_s1", "mean_wealth_s2", "mean_wealth_w",
                                   "mean_income_s1", "mean_income_s2", "mean_income_w",
                                   "mean_wealth", "mean_income",
                                   "capital_expenditures", "labor_expenditures", "consumption_expenditures",
                                   "unemployment_rate"]

    def initialize_simulation(self):
        # PARAMETER INITIALIZATION
        self.w = self.wage * (1 + self.payroll_tax)
        self.p = self.price * (1 + self.sales_tax)
        self.r = self.cap_price * (1 + self.turnover_tax)

        # POPULATION INITIALIZATION
        self.N_c = ceil(self.N_pop * self.percent_cap)
        self.N_w = self.N_pop - self.N_c
        self.N_c1 = ceil(self.N_c * self.percent_sec_1)
        self.N_c2 = self.N_c - self.N_c1

        self.pop = np.full(self.N_pop, self.init_wealth)
        self.pop_indices = np.arange(self.N_pop)
        self.cap_indices = np.arange(self.N_c)
        self.cap1_indices = np.arange(self.N_c1)
        self.cap2_indices = np.arange(self.N_c1, self.N_c)
        self.worker_indices = np.arange(self.N_c, self.N_pop)

        self.pop_indices_copy = np.copy(self.pop_indices)
        self.cap_indices_copy = np.copy(self.cap_indices)
        self.cap1_indices_copy = np.copy(self.cap1_indices)
        self.cap2_indices_copy = np.copy(self.cap2_indices)
        self.worker_indices_copy = np.copy(self.worker_indices)

        self.firm_stock = np.full(self.N_c, self.init_firm_endowment)
        self.labor_supply = np.full(self.N_w, self.max_jobs)
        self.supply = np.concatenate((self.firm_stock, self.labor_supply))

        # DATA INITIALIZATION
        self.wealth_dist = np.zeros(self.N_pop)
        self.income_dist = np.zeros(self.N_pop)

        self.wealth_gini_ts = np.zeros(self.steps)
        self.income_gini_ts = np.zeros(self.steps)
        self.mean_wealth_cap1_ts = np.zeros(self.steps)
        self.mean_wealth_cap2_ts = np.zeros(self.steps)
        self.mean_wealth_workers_ts = np.zeros(self.steps)
        self.mean_income_cap1_ts = np.zeros(self.steps)
        self.mean_income_cap2_ts = np.zeros(self.steps)
        self.mean_income_workers_ts = np.zeros(self.steps)
        self.mean_wealth_global_ts = np.zeros(self.steps)
        self.mean_income_global_ts = np.zeros(self.steps)
        self.deciles_wealth_cap1 = np.zeros((self.steps, 10))
        self.deciles_wealth_cap2 = np.zeros((self.steps, 10))
        self.deciles_wealth_workers = np.zeros((self.steps, 10))
        self.deciles_income_cap1 = np.zeros((self.steps, 10))
        self.deciles_income_cap2 = np.zeros((self.steps, 10))
        self.deciles_income_workers = np.zeros((self.steps, 10))
        self.deciles_wealth_global = np.zeros((self.steps, 10))
        self.deciles_income_global = np.zeros((self.steps, 10))
        self.k_expenditures_ts = np.zeros(self.steps)
        self.l_expenditures_ts = np.zeros(self.steps)
        self.c_expenditures_ts = np.zeros(self.steps)
        self.unemployment_ts = np.zeros(self.steps)

    def calculate_gini(self, x):
        y = np.sort(x)
        n = len(x)
        mu = np.mean(x)
        S = np.sum(np.fromiter(((n - i) * y[i] for i in range(n)), dtype=float))
        return (n + 1) / n - (2 / (mu * n * n)) * S

    def purchase_cycle(self, resource, buyer_price, seller_price, buyers, sellers, labor=False):
        R = np.copy(resource)
        expenditures = np.zeros(self.N_pop)
        income = np.zeros(self.N_pop)
        consumption = np.zeros(self.N_pop)
        np.random.shuffle(buyers)
        for i in buyers:
            np.random.shuffle(sellers)
            insufficient_stock = (R[i] > np.sum(self.supply[sellers] + consumption[sellers]))
            for j in sellers:
                if R[i] == 0 or (insufficient_stock and np.sum(self.supply[sellers] + consumption[sellers]) <= 0):
                    break
                cap = self.supply[j] + consumption[j]
                if labor:
                    purchase = min(R[i], cap, 1)
                else:
                    purchase = min(R[i], cap)
                expenditures[i] += buyer_price * purchase
                income[j] += seller_price * purchase
                consumption[j] -= purchase
                R[i] -= purchase
        return expenditures, income, consumption

    def capital_purchases(self, K):
        return self.purchase_cycle(K, self.r, self.cap_price, self.cap_indices_copy, self.cap2_indices_copy)

    def labor_purchases(self, L):
        return self.purchase_cycle(L, self.w, self.wage, self.cap_indices_copy, self.worker_indices_copy, labor=True)

    def good_purchases(self, C):
        return self.purchase_cycle(C, self.p, self.price, self.pop_indices_copy, self.cap1_indices_copy)

    def production(self, K, L):
        product = np.zeros(self.N_pop)
        for i in self.cap_indices:
            product[i] = self.A * (abs(K[i]) ** self.a) * (abs(L[i]) ** (1-self.a))
        return product

    def sim_step(self, wealth_vector, goods_vector, t):
        # Budget Selection RUle
        budgets = np.copy(wealth_vector)
        X = np.random.uniform(size=self.N_pop)
        expenditures = np.multiply(budgets, X)
        cap_expenditures, lab_expenditures = np.split(expenditures, [self.N_c])

        Y = np.random.uniform(size=self.N_c)
        investment = np.multiply(cap_expenditures, Y)
        K = self.a * investment / self.r
        L = (1 - self.a) * investment / self.w

        cap_consumption = cap_expenditures - investment
        consumption = np.concatenate((cap_consumption, lab_expenditures))
        C = consumption / self.p

        # Capital Acquisition Rule
        k_expenditures, k_income, k_consumption = self.capital_purchases(K)

        # Labor Acquisition Rule
        l_expenditures, l_income, l_consumption = self.labor_purchases(L)

        # Consumption Rule
        c_expenditures, c_income, c_consumption = self.good_purchases(C)

        # Production Rule
        Pi = self.production(k_expenditures[:self.N_c]/self.r, l_expenditures[:self.N_c]/self.w)

        # System Update Rule
        goods_vector += Pi + k_consumption + c_consumption
        wealth_vector += k_income + l_income + c_income - k_expenditures - l_expenditures - c_expenditures

        # Taxation Rule
        turnover_tax = np.sum(k_expenditures) - np.sum(k_income)
        payroll_tax = np.sum(l_expenditures) - np.sum(l_income)
        sales_tax = np.sum(c_expenditures) - np.sum(c_income)
        income_tax = self.income_tax * (k_income + l_income + c_income)
        wealth_vector -= income_tax
        wealth_tax = self.wealth_tax * wealth_vector
        wealth_vector -= wealth_tax
        redist_amt = (np.sum(wealth_tax) + np.sum(income_tax) + sales_tax + payroll_tax + turnover_tax) / self.N_pop
        redist_vector = np.full(self.N_pop, redist_amt)
        wealth_vector += redist_vector
        self.wealth_dist = wealth_vector

        # Data collection
        cap1_income = c_income[:self.N_c1]
        cap2_income = k_income[self.N_c1:self.N_c]
        worker_income = l_income[self.N_c:]
        self.income_dist = (1 - self.income_tax) * np.concatenate((cap1_income, cap2_income, worker_income))

        self.wealth_gini_ts[t] = self.calculate_gini(self.wealth_dist)
        self.income_gini_ts[t] = self.calculate_gini(self.income_dist)
        self.mean_wealth_cap1_ts[t] = np.mean(self.wealth_dist[self.cap1_indices])
        self.mean_wealth_cap2_ts[t] = np.mean(self.wealth_dist[self.cap2_indices])
        self.mean_wealth_workers_ts[t] = np.mean(self.wealth_dist[self.worker_indices])
        self.mean_income_cap1_ts[t] = np.mean(self.income_dist[self.cap1_indices])
        self.mean_income_cap2_ts[t] = np.mean(self.income_dist[self.cap2_indices])
        self.mean_income_workers_ts[t] = np.mean(self.income_dist[self.worker_indices])
        self.mean_wealth_global_ts[t] = np.mean(self.wealth_dist)
        self.mean_income_global_ts[t] = np.mean(self.income_dist)
        self.deciles_wealth_cap1[t] = np.percentile(self.wealth_dist[self.cap1_indices], np.arange(start=10, stop=101, step=10))
        self.deciles_wealth_cap2[t] = np.percentile(self.wealth_dist[self.cap2_indices], np.arange(start=10, stop=101, step=10))
        self.deciles_wealth_workers[t] = np.percentile(self.wealth_dist[self.worker_indices], np.arange(start=10, stop=101, step=10))
        self.deciles_income_cap1[t] = np.percentile(self.income_dist[self.cap1_indices], np.arange(start=10, stop=101, step=10))
        self.deciles_income_cap2[t] = np.percentile(self.income_dist[self.cap2_indices], np.arange(start=10, stop=101, step=10))
        self.deciles_income_workers[t] = np.percentile(self.income_dist[self.worker_indices], np.arange(start=10, stop=101, step=10))
        self.deciles_wealth_global[t] = np.percentile(self.wealth_dist, np.arange(start=10, stop=101, step=10))
        self.deciles_income_global[t] = np.percentile(self.income_dist, np.arange(start=10, stop=101, step=10))
        self.k_expenditures_ts[t] = np.sum(k_expenditures, dtype=float)
        self.l_expenditures_ts[t] = np.sum(l_expenditures, dtype=float)
        self.c_expenditures_ts[t] = np.sum(c_expenditures, dtype=float)
        self.unemployment_ts[t] = np.sum(np.fromiter((1/self.N_pop if wage == 0 else 0
                                                      for wage in l_income[self.worker_indices]), dtype=float))

    def simulate(self, *params):
        self.parameters.append(params)
        self.initialize_simulation()
        for i in np.arange(self.steps):
            self.sim_step(self.pop, self.supply, i)
        self.record_simulation_data()

    def record_simulation_data(self):
        self.all_wealth_dists.append(self.wealth_dist)
        self.all_income_dists.append(self.income_dist)

        self.decile_data = [self.deciles_wealth_cap1, self.deciles_wealth_cap2, self.deciles_wealth_workers,
                            self.deciles_income_cap1, self.deciles_income_cap2, self.deciles_income_workers,
                            self.deciles_wealth_global, self.deciles_income_global]
        for i, decile_data in enumerate(self.decile_data):
            self.all_decile_data[i].append(decile_data)

        self.ts_data = [self.wealth_gini_ts, self.income_gini_ts,
                        self.mean_wealth_cap1_ts, self.mean_wealth_cap2_ts, self.mean_wealth_workers_ts,
                        self.mean_income_cap1_ts, self.mean_income_cap2_ts, self.mean_income_workers_ts,
                        self.mean_wealth_global_ts, self.mean_income_global_ts,
                        self.k_expenditures_ts, self.l_expenditures_ts, self.c_expenditures_ts,
                        self.unemployment_ts]
        for i, ts_data in enumerate(self.ts_data):
            self.all_ts_data[i].append(ts_data)

    def export_pop_data(self):
        pop_data = [pd.DataFrame(i, index=self.parameters) for i in self.all_pop_data]
        return dict(zip(self.all_pop_data_titles, pop_data))

    def export_decile_data(self):
        decile_data = [pd.concat([pd.DataFrame(i).transpose() for i in j], keys=self.parameters) for j in self.all_decile_data]
        return dict(zip(self.all_decile_data_titles, decile_data))

    def export_ts_data(self):
        ts_data = [pd.DataFrame(i, index=self.parameters) for i in self.all_ts_data]
        return dict(zip(self.all_ts_data_titles, ts_data))

    def export_all_data(self):
        data = self.export_pop_data()
        data.update(self.export_decile_data())
        data.update(self.export_ts_data())
        return data
