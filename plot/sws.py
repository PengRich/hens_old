#coding=utf-8
from matplotlib.patches import Circle
import matplotlib.pyplot as plt
import os
import sys

sys.path.append("./")
from utils import Util


plt.rcParams["font.family"] = "Times New Roman"
class SWS(object):

    def __init__(self, fname, ID):
        ID = str(ID)
        self.fname = fname

        size = {
                "node_x": 0.5,
                "node_y": 0.5,
                  "left": 0.9,
                 "start": 0.3,
                   "end": 0.3,
                 "right": 1,
               "lmargin": 0.02,
               "rmargin": 0.08,
                   "top": 0,
                "bottom": 0,
                "radius": 0.12,
                  "font": 10.5,
                  "line": 0.6,
                   "eps": 0.01
               }
        self.size = {k: float(item) for k, item in size.iteritems()}
        self.info = {
                     "0": {"name":"2-2_test.json", "n_hs": 2, "n_cs": 2}
                    }
        self.locs = {
                     "HE": self.heat_exchanger,
                     "HU": self.utility,
                     "CU": self.utility
                    }
        self.topo = Util.read_json(fname)
        self.case = Util.read_json(os.path.join("case", self.info[ID]["name"]))
        self.case.update(self.info[ID])
        self.W, self.H = self._fig_size()
        self.term_loc  = self._terminal_loc()

    def _terminal_loc(self):

        loc = {"0": {}, "-1": {}}

        x = self.W - self.size["right"] - self.size["end"]
        y = self.H - self.size["top"] - self.size["node_y"]
        for i in range(int(self.case["n_hs"])):
            loc["-1"][str(i+1)] = [x, y]
            y -= self.size["node_y"]

        x = self.size["left"] + self.size["start"]
        y = self.size["bottom"] + self.size["node_y"]
        for i in range(int(self.case["n_cs"]), 0, -1):
            loc["0"][str(i)] = [x, y]
            y += self.size["node_y"]

        return loc

    def _fig_size(self):

        num_he =  len([he for he in self.topo if he["type"] == "HE"])
        num_he += len([he for he in self.topo if int(he["st"]) ==  0]) >= 1
        num_he += len([he for he in self.topo if int(he["st"]) == -1]) >= 1
        internal_u = []
        for he in self.topo:
            if he["type"] not in ["HU", "CU"] or not {"hs", "cs"}.issubset(he):
                continue
            if [he["st"], he["hs"], he["cs"]] not in internal_u:
                internal_u.append([he["st"], he["hs"], he["cs"]])
        num_he += len(internal_u)
        wide   = self.size["node_x"]*(num_he-1)+self.size["left"]+\
            self.size["right"]+self.size["start"]+self.size["end"]
        height = self.size["node_y"]*(self.case["n_hs"]+self.case["n_cs"]+1)+\
            self.size["top"]+self.size["bottom"]

        return wide, height

    def _arrow(self, ax, x, y, dx, dy):

        ax.arrow(x, y, dx, dy, head_width=0.06, head_length=0.06, alpha=1,
                 color="k", fc="k", ec="k", lw=self.size["line"], zorder=1)

        return ax

    def heat_exchanger(self, x, he):
        y0 = self.H-self.size["top"]-int(he["hs"])*self.size["node_y"]
        y1 = self.H-self.size["top"]-\
            (self.case["n_hs"]+self.case["n_cs"])*self.size["node_y"]

        return x+self.size["node_x"], [[x, x], [y0, y1]]

    def utility(self, x, he):

        oppo = {"H": "C", "C": "H"}

        if str(he["st"]) in ["-1", "0"]:
            ID  = str(he["hs"]) if "hs" in he else str(he["cs"])
            loc = self.term_loc[str(he["st"])][ID]
            return x, [[loc[0]], [loc[1]]]

        if he["side"] == "H":
            y = self.H-self.size["top"]-int(he["hs"])*self.size["node_y"]
        else:
            y = self.H-self.size["top"]-\
                (self.case["n_hs"]+self.case["n_cs"])**self.size["node_y"]

        if [he["st"], he["hs"], he["cs"]] in self.inter:
            return x, [[], []]
        self.inter.append([he["st"], he["hs"], he["cs"]])

        u = [ex for ex in self.topo if "side" in ex and ex["side"] == oppo[he["side"]]]

        pair = [ex for ex in u if ex["hs"]==he["hs"] and ex["cs"]==he["cs"] and
                ex["st"]==he["st"]]
        if pair: return self.heat_exchanger(x, he)

        return x+self.size["node_x"], [[x], [y]]

    def plot(self):

        plt.figure(figsize=(self.W, self.H))
        ax = plt.gca()
        ax.axis("equal")
        ax.set_xlim(0, self.W)
        ax.set_ylim(0, self.H)
        ax.set_xticks([])
        ax.set_yticks([])

        wide = self.W - self.size["left"] - self.size["right"]
        move = self.size["radius"] / 2.0 + self.size["eps"]
        head = r"%s %s$^{\circ}$C"
        foot = r"%s$^{\circ}$C %skg/s"

        y = self.H - self.size["top"] - self.size["node_y"]
        for s in range(self.case["n_hs"]):
            k  = "H"+str(s+1)
            ax.text(self.size["lmargin"], y-move, head % (k, str(self.case[k]["in"])),
                    fontsize=self.size["font"], zorder=3)
            ax.text(self.W-self.size["right"]+self.size["rmargin"], y-move,
                    foot % (str(self.case[k]["out"]), str(self.case[k]["F"])),
                    fontsize=self.size["font"], zorder=3)
            ax = self._arrow(ax, self.size["left"], y, wide, 0)
            y -= self.size["node_y"]

        y = self.size["bottom"]+self.size["node_y"]
        x = self.W - self.size["right"]
        for s in range(self.case["n_cs"]):
            k  = "C"+str(self.case["n_cs"]-s)
            ax.text(self.size["lmargin"], y-move, head % (k, str(self.case[k]["out"])),
                    fontsize=self.size["font"], zorder=3)
            ax.text(self.W-self.size["right"]+self.size["rmargin"], y-move,
                    foot % (str(self.case[k]["in"]), str(self.case[k]["F"])),
                    fontsize=self.size["font"], zorder=3)
            ax = self._arrow(ax, x, y, -wide, 0)
            y += self.size["node_y"]

        x  = self.size["left"]+self.size["node_x"]+self.size["start"]
        xs = []
        ys = []
        self.inter = []
        for he in self.topo:
             x, loc = self.locs[he["type"]](x, he)
             xs.extend(loc[0])
             ys.extend(loc[1])

             if he["type"] == "HE":
                 ax.plot(loc[0], loc[1], color="black",
                         linewidth=self.size["line"], zorder=0)
                 continue

             if not len(loc[0]):continue

             if len(loc[0]) == 1:
                 typ  = "C" if he["type"] == "CU" else "H"
                 ax.text(loc[0][0]-move, loc[1][0]-move, typ,
                         fontsize=self.size["font"], zorder=3)
             elif he["Q"] > 0:
                 ax.text(loc[0][0]-move, max(loc[1])-move, "C",
                         fontsize=self.size["font"], zorder=3)
                 ax.text(loc[0][0]-move, min(loc[1])-move, "H",
                         fontsize=self.size["font"], zorder=3)
             else:
                 ax.text(loc[0][0]-move, max(loc[1])-move, "H",
                         fontsize=self.size["font"], zorder=3)
                 ax.text(loc[0][0]-move, min(loc[1])-move, "C",
                         fontsize=self.size["font"], zorder=3)

        for i, x in enumerate(xs):
            he = Circle((x, ys[i]), self.size["radius"], alpha=1, color="grey",
                        fc="w", ec="k", lw=self.size["line"], zorder=2)
            ax.add_artist(he)

        ax.axis('off')
        fname = self.fname.replace(".json", ".eps")
        plt.savefig(fname, format='eps', dpi=1000, bbox_inches="tight")
        plt.show()


if __name__ == "__main__":
    SWS("result/example.json", "0").plot()
