#coding=utf-8
import math

_types = {
          "hs": "hot stream",
          "cs": "cold stream",
          "hu": "hot utility",
          "cu": "cold utility"
         }

KINDS = {
         "1": "hot stream",
         "2": "cold stream",
         "3": "steam",
         "4": "cooling water"
        }

HOT_UTILITIES  = ["steam"]
COLD_UTILITIES = ["cooling water"]

PEN_F = 1.e7
def PEN(excess):
    return 0.5*PEN_F*(max(0, -excess+1)**2)


class Thermodynamics(object):

    def __init__(self):
        pass

    @staticmethod
    def heat_exchange_coeff(hs, cs):
        return hs.h*cs.h / (hs.h+cs.h)

    @staticmethod
    def heat_exchange_potential(s):
        return abs((s.T_hin-s.T_out)*s.HCpF)

    @staticmethod
    def heat_exchange_area(Q, K, dtl, dtr):
        dtm = (dtl-dtr)/math.log(dtl/dtr) if abs(dtl-dtr) > 1.e-3 else (dtl+dtr)/2.0
        return Q/dtm/K
