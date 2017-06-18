# coding=utf-8
from math import log

from py.utils import Util
from py.config import *

TB   = Thermodynamics()

class CostFactor(object):

    def __init__(self, f_cu, f_hu, f_A):

        self.f_cu = float(f_cu)
        self.f_hu = float(f_hu)
        self.f_A  = map(float, f_A)


class Result(object):

    def __init__(self, factors):
        self.f = factors
        self._init()

    def _init(self):

        self.tac = 0

        self.cost_A  = 0
        self.cost_hu = 0
        self.cost_cu = 0

        self.penalty = None

        self.A    = None
        self.Q_hu = None
        self.Q_cu = None

    def _update(self):

        if self.A:
            sta = 1 if self.A>1.e-3 else 0
            self.cost_A = sta*self.f.f_A[0] + self.f.f_A[1]*(self.A**self.f.f_A[2])
        else:
            self.cost_A = 0

        self.cost_hu  = self.f.f_hu*abs(self.Q_hu) if self.Q_hu else 0
        self.cost_cu  = self.f.f_cu*abs(self.Q_cu) if self.Q_cu else 0

        self.tac = sum([self.cost_A, self.cost_hu, self.cost_cu])

        if self.penalty: self.tac += self.penalty


class Stream(object):

    def __init__(self, ID, T_in, T_out, HCpF, h, kind):
        self.ID    = str(ID)
        self.T_in  = float(T_in)
        self.T_out = float(T_out)
        self.HCpF  = float(HCpF) if HCpF else None
        self.h     = float(h)
        self.kind  = KINDS[str(kind)]

        self.potential = abs((T_in-T_out)*HCpF) if HCpF else None


class HeatExchanger(Result):

    def __init__(self, hot_stream, cold_stream, factors):

        Result.__init__(self, factors)

        self.A = None
        self.Q = None

        self.sta = False

        self.T_hin  = None
        self.T_hout = None
        self.T_cin  = None
        self.T_cout = None

        self.hs   = hot_stream
        self.cs   = cold_stream
        self.K_he = hot_stream.h*cold_stream.h / (hot_stream.h+cold_stream.h)

        self.potential = min(hot_stream.potential, cold_stream.potential) \
            if hot_stream.potential and cold_stream.potential else None

        self.result = Result(factors)


class Utility(HeatExchanger):

    def __init__(self, stream, ustream, factors):

        if ustream.kind not in HOT_UTILITIES and \
                ustream.kind not in COLD_UTILITIES:
            Util.logger.info("Error: Unknown utility kind")
            return None

        if ustream.kind in HOT_UTILITIES:
            HeatExchanger.__init__(self, ustream, stream, factors)
            self.T_hin  = ustream.T_in
            self.T_hout = ustream.T_out

        if ustream.kind in COLD_UTILITIES:
            HeatExchanger.__init__(self, stream, ustream, factors)
            self.T_cin  = ustream.T_in
            self.T_cout = ustream.T_out


class FlexibleUtility(object):

    def __init__(self, stream, hot_ustreams, cold_ustreams, factors):

        self.utility   = None
        self.utilities = None
        self.stream    = stream

        self.hus = {k: Utility(stream, u, factors)
                    for k, u in hot_ustreams.iteritems()}
        self.cus = {k: Utility(stream, u, factors)
                    for k, u in cold_ustreams.iteritems()}

    def _update_utility(self):

        if not self.utilities:
            Util.logger.info("Error: utilities is None")
            return None

        min_cost = 1.e20
        for kind in self.utilities:
            dtl = self.utilities[kind].T_hin - self.utilities[kind].T_cout
            dtr = self.utilities[kind].T_hout - self.utilities[kind].T_cin
            if dtl>0 and dtr>0:
                self.utilities[kind].A = \
                    TB.heat_exchange_area(abs(self.utilities[kind].Q),
                                          self.utilities[kind].K_he, dtl, dtr)
                self.utilities[kind].penalty = None
                if kind in HOT_UTILITIES:
                    self.utilities[kind].Q_hu = self.utilities[kind].Q
                    self.utilities[kind].Q_cu = None
                else:
                    self.utilities[kind].Q_hu = None
                    self.utilities[kind].Q_cu = self.utilities[kind].Q
            else:
                self.utilities[kind].A    = None
                self.utilities[kind].Q_hu = None
                self.utilities[kind].c_hu = None
                self.utilities[kind].penalty = PEN(dtl) + PEN(dtr)

            self.utilities[kind]._update()
            if self.utilities[kind].tac < min_cost:
                self.utility = self.utilities[kind]
                min_cost     = self.utilities[kind].tac


class FlexibleExchanger(HeatExchanger):

    def __init__(self, hot_stream, cold_stream, hot_ustreams, cold_ustreams,
                 factors):

        HeatExchanger.__init__(self, hot_stream, cold_stream, factors)
        self.hu   = None
        self.cu   = None

        self.hsu = FlexibleUtility(hot_stream, hot_ustreams, cold_ustreams, factors)
        self.csu = FlexibleUtility(cold_stream, hot_ustreams, cold_ustreams, factors)

    def _update_cost(self):
        dtl = self.T_hin - self.T_cout
        dtr = self.T_hout - self.T_cin
        if dtl>0 and dtr>0:
            self.A    = TB.heat_exchange_area(abs(self.Q), self.K_he, dtl, dtr)
            self.Q_hu = None
            self.C_hu = None
            self.penalty = None
            self._update()
        else:
            if self.Q > 0:
                self.hsu.utilities = self.hsu.cus
                for kind in self.hsu.utilities:
                    self.hsu.utilities[kind].T_hin  = self.T_hin
                    self.hsu.utilities[kind].T_hout = self.T_hout
                    self.hsu.utilities[kind].Q      = self.Q
                self.hsu._update_utility()
                self.cu = self.hsu.utility

                self.csu.utilities = self.csu.hus
                for kind in self.csu.utilities:
                    self.csu.utilities[kind].T_cin  = self.T_cin
                    self.csu.utilities[kind].T_cout = self.T_cout
                    self.csu.utilities[kind].Q      = self.Q
                self.csu._update_utility()
                self.hu = self.csu.utility

            else:
                self.csu.utilities = self.csu.cus
                for kind in self.csu.utilities:
                    self.csu.utilities[kind].T_hin  = self.T_cin
                    self.csu.utilities[kind].T_hout = self.T_cout
                    self.csu.utilities[kind].Q      = self.Q
                self.csu._update_utility()
                self.cu = self.csu.utility

                self.hsu.utilities = self.hsu.hus
                for kind in self.hsu.utilities:
                    self.hsu.utilities[kind].T_cin  = self.T_hin
                    self.hsu.utilities[kind].T_cout = self.T_hout
                    self.hsu.utilities[kind].Q      = self.Q
                self.hsu._update_utility()
                self.hu = self.hsu.utility
            self.tac = self.hu.tac + self.cu.tac
            self.cost_A  = self.hu.cost_A + self.cu.cost_A
            self.cost_hu = self.hu.cost_hu + self.cu.cost_hu
            self.cost_cu = self.hu.cost_cu + self.cu.cost_cu


class FlexibleUtilityHens(object):

    def __init__(self, case_data, n_st):

        # exchangers
        self.exchangers = {}
        self.terminals  = {}
        self.utilities  = []

        # hens investment and operation
        self.total_A  = 0.0
        self.total_hu = 0.0
        self.total_cu = 0.0

        # basic info
        self.n_hs    = int(case_data["info"]["hs"])
        self.n_cs    = int(case_data["info"]["cs"])
        self.n_st    = n_st
        self.n_s     = self.n_hs+self.n_cs
        self.n_st_he = self.n_hs*self.n_cs
        self.n_he    = self.n_st_he*n_st

        # topology info
        self.stas  = [0]*(self.n_he+1)
        self.Qs    = [0.0]*(self.n_he+1)
        self.init_stream_loc()
        self.init_utility_loc()

        # cost
        self.factors = CostFactor(case_data["cost"]["cu"],
                                  case_data["cost"]["hu"],
                                  case_data["cost"]["area"])

        self.result  = Result(self.factors)

        # streams
        self.hot_streams   = \
            {ID: Stream(ID, hs["T_in"], hs["T_out"], hs["HCpF"], hs["h"], hs["kind"])
             for ID, hs in case_data["streams"]["hs"].iteritems()}
        self.cold_streams  = \
            {ID: Stream(ID, cs["T_in"], cs["T_out"], cs["HCpF"], cs["h"], cs["kind"])
             for ID , cs in case_data["streams"]["cs"].iteritems()}
        self.hot_ustreams  = \
            {KINDS[hu["kind"]]: Stream(ID, hu["T_in"], hu["T_out"], None, hu["h"], hu["kind"])
             for ID, hu in case_data["streams"]["hu"].iteritems()}
        self.cold_ustreams = \
            {KINDS[cu["kind"]]: Stream(ID, cu["T_in"], cu["T_out"], None, cu["h"], cu["kind"])
             for ID, cu in case_data["streams"]["cu"].iteritems()}

        for i in range(self.n_hs):
            self.terminals[str(i+1)] = FlexibleUtility(self.hot_streams[str(i+1)],
                                                       self.hot_ustreams,
                                                       self.cold_ustreams,
                                                       self.factors)
        for j in range(self.n_cs):
            i = self.n_hs + j
            self.terminals[str(i+1)] = FlexibleUtility(self.cold_streams[str(j+1)],
                                                       self.hot_ustreams,
                                                       self.cold_ustreams,
                                                       self.factors)

        for k in range(self.n_st):
            for i in range(self.n_hs):
                for j in range(self.n_cs):
                    ID = str(k*self.n_hs*self.n_cs + i*self.n_cs + j+1)
                    self.exchangers[ID] = FlexibleExchanger(self.hot_streams[str(i+1)],
                                                            self.cold_streams[str(j+1)],
                                                            self.hot_ustreams,
                                                            self.cold_ustreams,
                                                            self.factors)

    def init_stream_loc(self):
        self.hloc  = {str(i+1): [] for i in range(self.n_hs)}
        self.cloc  = {str(i+1): [] for i in range(self.n_cs)}

    def init_utility_loc(self):
        self.uloc  = []
