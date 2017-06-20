#coding=utf-8
from threading import Lock
from threading import Timer
from threading import currentThread
from threading import Thread
from datetime import datetime
import random
import os
# import numpy as np

from py.simulator.without_split import WithoutSplit
from py.utils import Util


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
        self.lock = Lock
        self.simulator = WithoutSplit(self.case, self.n_st)

        result        = {"tac": [], "Qs": []}
        self.filename = os.path.join("py", "solution.json")
        Util.write_json(self.filename, result)

    def multi_threads(self, num_threads=10):

        for i in range(num_threads):
            print "Thread, %s", str(i)
            r = random.Random()
            Timer(5+i*2, self.single_thread, [r, True]).start()

    def single_thread(self, r=None, wakeup=False):

        if not r: r = random.Random(1)

        simulator = WithoutSplit(self.case, self.n_st)
        Qs0  = [float(self.initial_Q)*r.random() for i in range(simulator.hens.n_he+1)]
        tac0 = simulator.tac(Qs0)

        tac_min = tac0 if tac0 < 1.e20 else 1.e20

        ite     = 1
        rej_ite = 0
        # while ite <= self.ite_max:
        while True:
            ite += 1

            Qs1 = [Q+(1-2*r.random())*self.dL*r.random() for Q in Qs0]
            Qs2 = [Q if Q >= self.dL*self.remove_rate else 0.0 for Q in Qs1]
            tac = simulator.tac(Qs2)

            if tac < tac_min:
                tac_min = tac
                Qs_min  = Qs2[:]
                num_hes = len([Q for Q in Qs_min[1:] if abs(Q) > 1.e-3])
                if ite > 5.e4: print currentThread(), ite, tac_min, num_hes
                rej_ite = 0
            else:
                rej_ite += 1

            if rej_ite > self.max_rej_ite: break

            if tac < tac0 or r.random() < self.accept_rate:
                Qs0  = Qs2[:]
                tac0 = tac

        with self.lock:
            self.record_solution(tac_min, Qs_min)
        if wakeup: Timer(5, self.single_thread, [r, True]).start()

    def record_solution(tac_min, Qs_min):

        result = Util.read_json(self.filename)

        if not result["tac"] or tac_min < result["tac"]:
            result = {"tac": tac_min, "Q": Qs_min[1:]}
            Util.write_json(self.filename, result)



if __name__ == "__main__":
    RWCE("10sp2", 4).multi_threads(3)
