#coding=utf-8
from datetime import datetime
from multiprocessing import Pool
from multiprocessing import cpu_count
from multiprocessing import current_process
from multiprocessing import Lock

import random
import os
import time
# import numpy as np

from py.simulator.without_split import WithoutSplit
from py.utils import Util


def run(case_name, n_st):

    r = random.Random()
    RWCE(case_name, n_st).single_thread(r)


Filename = os.path.join("py", "solution.json")
lock     = Lock()
def multi_process(case_name, n_st):
    result = {"tac": [], "Qs": []}
    Util.write_json(Filename, result)

    cpus = cpu_count() - 2
    pool = Pool(processes=cpus)
    results = []

    for i in xrange(0, cpus):
        print "Start ", i
        results.append(pool.apply_async(run, (case_name, n_st)))

    pool.close()
    pool.join()

    for result in results:
        print result.get()


class RWCE():

    dL          = 100
    ite_max     = 1.e7
    remove_rate = 0.9
    initial_Q   = 8000
    accept_rate = 0.01
    max_rej_ite = 1.e5

    def __init__(self, case_name, n_st):
        self.case = case_name
        self.n_st = n_st
        self.simulator = WithoutSplit(self.case, self.n_st)

        # result        = {"tac": [], "Qs": []}
        # self.filename = os.path.join("py", "solution.json")
        # Util.write_json(self.filename, result)

    def single_thread(self, r=None):

        if not r: r = random.Random(1)

        simulator = WithoutSplit(self.case, self.n_st)
        Qs0  = [float(self.initial_Q)*r.random() for i in range(simulator.hens.n_he+1)]
        tac0 = simulator.tac(Qs0)

        tac_min = tac0 if tac0 < 1.e20 else 1.e20
        print current_process().name, "start"

        ite     = 1
        rej_ite = 0
        # while ite <= self.ite_max:
        while True:
            ite += 1
            num_hes = len([Q for Q in Qs0[1:] if abs(Q) > 1.e-3])
            if ite%50000 == 0: print current_process().name, ite, tac_min, num_hes

            Qs1 = [Q+(1-2*r.random())*self.dL*r.random() for Q in Qs0]
            Qs2 = [Q if Q >= self.dL*self.remove_rate else 0.0 for Q in Qs1]
            tac = simulator.tac(Qs2)

            if tac < tac_min:
                tac_min = tac
                Qs_min  = Qs2[:]
                num_hes = len([Q for Q in Qs_min[1:] if abs(Q) > 1.e-3])
                if ite > 5.e5: print "Best", current_process().name, ite, tac_min, num_hes
                rej_ite = 0
            else:
                rej_ite += 1

            if rej_ite > self.max_rej_ite: break

            if tac < tac0 or r.random() < self.accept_rate:
                Qs0  = Qs2[:]
                tac0 = tac

        lock.acquire()
        self.record_solution(tac_min, Qs_min)
        lock.release()
        num_hes = len([Q for Q in Qs_min[1:] if abs(Q) > 1.e-3])
        print "Result", current_process().name, ite, tac_min, num_hes
        return tac_min

    def record_solution(tac_min, Qs_min):

        result = Util.read_json(Filename)

        if not result["tac"] or tac_min < result["tac"]:
            result = {"tac": tac_min, "Q": Qs_min[1:]}
            Util.write_json(Filename, result)



if __name__ == "__main__":
    RWCE("10sp2", 4).multi_threads(3)
