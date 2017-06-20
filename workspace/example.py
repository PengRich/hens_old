# coding=utf-8
import os
import sys
sys.path.append("./")
from py.simulator.without_split import WithoutSplit as wos
from py.optimizer.rwce import RWCE

def example1(case_name="9sp1", n_st=3):

    s = wos(case_name, n_st)
    Qs = [0]*(s.hens.n_he+1)
    Qs[1] = 18542.0
    Qs[10] = 9030.0
    Qs[12] = 1276.0
    Qs[14] = 5093.0
    Qs[17] = 580.0
    Qs[18] = 10428.0
    Qs[27] = 570.0
    Qs[36] = 1458.0
    Qs[42] = 6604.0
    Qs[58] = 8122.0
    Qs[59] = 833.0

    # s.hens.Qs[1-1]  = 1
    # s.hens.Qs[10-1] = 1
    # s.hens.Qs[12-1] = 1
    # s.hens.Qs[14-1] = 1
    # s.hens.Qs[17-1] = 1
    # s.hens.Qs[18-1] = 1
    # s.hens.Qs[27-1] = 1
    # s.hens.Qs[36-1] = 1
    # s.hens.Qs[42-1] = 1
    # s.hens.Qs[58-1] = 1
    # s.hens.Qs[59-1] = 1
    print s.tac(Qs)
    print s.hens.result.cost_A
    print s.hens.result.cost_hu
    print s.hens.result.cost_cu
    print s.hens.result.penalty
    exit()


def example2(case_name="10sp2", n_st=4):
    s = wos(case_name, n_st)
    Qs = [0]*(s.hens.n_he+1)
    Qs[6] = 1618.0
    Qs[10] = 740.0
    Qs[18] = 3642.0
    Qs[23] = 1151.0
    Qs[33] = 279.5
    Qs[39] = 11975.0
    Qs[41] = 2358.0
    Qs[47] = 4874.0
    Qs[48] = 1073.0
    Qs[49] = 1991.0
    Qs[56] = 1429.0
    Qs[57] = 471.0
    Qs[69] = 402.0
    Qs[73] = 1499.0
    Qs[76] = 1614.0
    Qs[88] = 525.0
    print s.tac(Qs)
    print s.hens.result.tac
    print s.hens.result.cost_A
    print s.hens.result.cost_hu
    print s.hens.result.cost_cu
    print s.hens.result.penalty
    print s.tac()
    exit()


def test_rwce():
    RWCE("10sp2", 4).multi_threads(4)


if __name__ == "__main__":
    test_rwce()
