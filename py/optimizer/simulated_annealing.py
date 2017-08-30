#coding=utf-8
import random

class SimulatedAnnealing(object):

    def __init__(self):
        self.L_low  = 100000
        self.al     = 0.9
        self.ep_low = 1.0
        self.beta   = 0
        self.T0     = 10000.0
        self.r      = random.Random()

    def test(self, n_x, limit, y, y0):
        pass




