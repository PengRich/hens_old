# coding=utf-8
# from __future__ import division
from py.simulator.base import HenBase
from py.simulator.models import FlexibleExchanger


class WithoutSplit(object):

    def __init__(self, case_name, n_st):
        self.hens  = HenBase.init_case(case_name, n_st)
        self.T_OUT = {str(i+1): None for i in range(self.hens.n_hs+self.hens.n_cs)}

    def _update_tac(self):

        self.hens.result._init()
        for i in range(self.hens.n_hs):
            h_id  = str(i+1)
            for ID in self.hens.hloc[h_id]:
                self.hens.exchangers[ID]._update_cost()
                self.hens.result.tac     += self.hens.exchangers[ID].tac
                self.hens.result.cost_A  += self.hens.exchangers[ID].cost_A
                self.hens.result.cost_hu += self.hens.exchangers[ID].cost_hu
                self.hens.result.cost_cu += self.hens.exchangers[ID].cost_cu

        for ID in self.hens.uloc:
            self.hens.terminals[ID]._update_utility()
            self.hens.result.tac     += self.hens.terminals[ID].utility.tac
            self.hens.result.cost_A  += self.hens.terminals[ID].utility.cost_A
            self.hens.result.cost_hu += self.hens.terminals[ID].utility.cost_hu
            self.hens.result.cost_cu += self.hens.terminals[ID].utility.cost_cu

    def _update_exchanger_temperature(self):

        for i in range(self.hens.n_hs):
            h_id = str(i+1)
            if not self.hens.hloc[h_id]: continue
            ids   = self.hens.hloc[h_id]
            he_id = int(ids[0])
            self.hens.exchangers[ids[0]].Q   = self.hens.Qs[he_id]
            # self.hens.exchangers[ids[0]].sta = True
            if self.hens.Qs[he_id] > 0:
                self.hens.exchangers[ids[0]].T_hin  = self.hens.hot_streams[h_id].T_in
                self.hens.exchangers[ids[0]].T_hout = self.hens.exchangers[ids[0]].T_hin - self.hens.Qs[he_id] / self.hens.hot_streams[h_id].HCpF
            else:
                self.hens.exchangers[ids[0]].T_cin  = self.hens.hot_streams[h_id].T_in
                self.hens.exchangers[ids[0]].T_cout = self.hens.exchangers[ids[0]].T_hin - self.hens.Qs[he_id] / self.hens.hot_streams[h_id].HCpF

            for j, ID in enumerate(ids[1:]):
                he_id = int(ID)
                self.hens.exchangers[ID].Q   = self.hens.Qs[he_id]
                # self.hens.exchangers[ID].sta = True
                if self.hens.Qs[he_id] > 0:
                    self.hens.exchangers[ID].T_hin  = self.hens.exchangers[ids[j]].T_hout
                    self.hens.exchangers[ID].T_hout = self.hens.exchangers[ID].T_hin - self.hens.Qs[he_id] / self.hens.hot_streams[h_id].HCpF
                else:
                    self.hens.exchangers[ID].T_cin  = self.hens.exchangers[ids[j]].T_hout
                    self.hens.exchangers[ID].T_cout = self.hens.exchangers[ID].T_hin - self.hens.Qs[he_id] / self.hens.hot_streams[h_id].HCpF

        for i in range(self.hens.n_cs):
            c_id = str(i+1)
            if not self.hens.cloc[c_id]: continue
            ids   = list(reversed(self.hens.cloc[c_id]))
            he_id = int(ids[0])
            if self.hens.Qs[he_id] > 0:
                self.hens.exchangers[ids[0]].T_cin  = self.hens.cold_streams[c_id].T_in
                self.hens.exchangers[ids[0]].T_cout = self.hens.exchangers[ids[0]].T_cin + self.hens.Qs[he_id] / self.hens.cold_streams[c_id].HCpF
            else:
                self.hens.exchangers[ids[0]].T_hin  = self.hens.cold_streams[c_id].T_in
                self.hens.exchangers[ids[0]].T_hout = self.hens.exchangers[ids[0]].T_cin + self.hens.Qs[he_id] / self.hens.cold_streams[c_id].HCpF

            for j, ID in enumerate(ids[1:]):
                he_id = int(ID)
                if self.hens.Qs[he_id] > 0:
                    self.hens.exchangers[ID].T_cin  = self.hens.exchangers[ids[j]].T_cout
                    self.hens.exchangers[ID].T_cout = self.hens.exchangers[ID].T_cin + self.hens.Qs[he_id] / self.hens.cold_streams[c_id].HCpF
                else:
                    self.hens.exchangers[ID].T_hin  = self.hens.exchangers[ids[j]].T_cout
                    self.hens.exchangers[ID].T_hout = self.hens.exchangers[ID].T_cin + self.hens.Qs[he_id] / self.hens.cold_streams[c_id].HCpF

    def _update_terminal_utility(self):

        self.hens.init_utility_loc()

        Q_us = []
        for i in range(self.hens.n_hs):
            h_id = str(i+1)
            self.T_OUT[h_id] = self.hens.exchangers[self.hens.hloc[h_id][-1]].T_hout if self.hens.hloc[h_id] else self.hens.hot_streams[h_id].T_in
            Q_us.append((self.T_OUT[h_id]-self.hens.hot_streams[h_id].T_out)*self.hens.hot_streams[h_id].HCpF)
        for j in range(self.hens.n_cs):
            c_id = str(j+1)
            i    = str(self.hens.n_hs+j+1)
            self.T_OUT[i] = self.hens.exchangers[self.hens.cloc[c_id][0]].T_cout if self.hens.cloc[c_id] else self.hens.cold_streams[c_id].T_in
            Q_us.append((self.T_OUT[i]-self.hens.cold_streams[c_id].T_out)*self.hens.cold_streams[c_id].HCpF)

        for i, Q_u in enumerate(Q_us):
            if abs(Q_u) < 1.e-3: continue
            ID = str(i+1)
            self.hens.uloc.append(ID)

            if Q_u > 0:
                self.hens.terminals[ID].utilities = self.hens.terminals[ID].cus
                for kind in self.hens.terminals[ID].utilities:
                    self.hens.terminals[ID].utilities[kind].T_hin  = self.T_OUT[ID]
                    self.hens.terminals[ID].utilities[kind].T_hout = self.hens.terminals[ID].stream.T_out
                    self.hens.terminals[ID].utilities[kind].Q      = Q_u
            else:
                self.hens.terminals[ID].utilities = self.hens.terminals[ID].hus
                for kind in self.hens.terminals[ID].utilities:
                    self.hens.terminals[ID].utilities[kind].T_cin  = self.T_OUT[ID]
                    self.hens.terminals[ID].utilities[kind].T_cout = self.hens.terminals[ID].stream.T_out
                    self.hens.terminals[ID].utilities[kind].Q      = Q_u

    def _to_stream_id(self, he_id):

        mod_he_id = int(he_id)%self.hens.n_st_he
        he_id_st  = mod_he_id if mod_he_id else self.hens.n_st_he
        mod_n_cs  = he_id_st % self.hens.n_cs
        h_id      = he_id_st/self.hens.n_cs+1 if mod_n_cs else he_id_st/self.hens.n_cs
        c_id      = he_id_st - (h_id-1)*self.hens.n_cs

        return str(h_id), str(c_id)

    def _update_topo(self, Qs):

        self.hens.Qs   = Qs[:]
        self.hens.stas = [1 if abs(Q) > 1.e-3 else 0 for Q in Qs]
        he_ids         = [i+1 for i, j in enumerate(self.hens.stas[1:])
                          if int(j)]

        self.hens.init_stream_loc()

        for he_id in he_ids:
            h_id, c_id = self._to_stream_id(he_id)
            self.hens.hloc[h_id].append(he_id)
            self.hens.cloc[c_id].append(he_id)

        self.hens.hloc = {key: map(str, list(sorted(he_ids)))
                          for key, he_ids in self.hens.hloc.iteritems()}
        self.hens.cloc = {key: map(str, list(sorted(he_ids)))
                          for key, he_ids in self.hens.cloc.iteritems()}

    def tac(self, Qs=None):

        # self.hens.total_Q_hu = 0.0
        # self.hens.total_Q_cu = 0.0
        # self.hens.total_A  = 0.0

        if Qs: self._update_topo(Qs)
        self._update_exchanger_temperature()
        self._update_terminal_utility()
        self._update_tac()

        return self.hens.result.tac


if __name__ == "__main__":
    pass
