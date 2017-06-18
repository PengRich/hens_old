# coding=utf-8
import os

from py.utils import Util
from py.simulator.models import FlexibleUtilityHens


class HenBase(object):

    def __init__(self):
        pass

    @staticmethod
    def init_case(case_name, n_st):
        filename  = os.path.join("py", "case", case_name+".json")
        json_data = Util.read_json(filename)
        case_data = {
                        "info": json_data["info"],
                        "cost": json_data["cost"],
                     "streams": {},
                    }

        def _stream(stream):
            return {"T_in": float(stream[1]), "T_out": float(stream[2]),
                    "HCpF": float(stream[3]), "h": float(stream[4]),
                    "kind": str(stream[5])}
        def _utility(stream):
            return {"T_in": float(stream[1]), "T_out": float(stream[2]),
                    "h": float(stream[3]), "kind": str(stream[4])}

        streams = {kind: [s for s in json_data["streams"] if s[0] == kind]
                   for kind in ["hs", "cs", "hu", "cu"]}

        s = {kind: {str(i+1): _stream(j) for i, j in enumerate(streams[kind])}
             for kind in ["hs", "cs"]}
        case_data["streams"].update(s)
        u = {kind: {str(i+1): _utility(j) for i, j in enumerate(streams[kind])}
             for kind in ["hu", "cu"]}
        case_data["streams"].update(u)
        # print str(case_data).replace("'", '"')

        return FlexibleUtilityHens(case_data, n_st)


