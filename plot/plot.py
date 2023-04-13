import numpy as np
import matplotlib.pyplot as plt
import json as j
import re
from itertools import groupby
from operator import itemgetter
import sys

def error(text):
    raise Exception(text)

# def groupby(iterable, key): # to do. remove
#     return tuple((a, tuple(b)) for (a, b) in itertools.groupby(iterable, key))

def group_sorted(iterable, key):
    return groupby(sorted(iterable, key=key), key=key)

def transpose_dictionaries(dictionary):
    """
    transpose a dictionary of dictionaries. for example, turn
    `{"a": {"c": 0, "d": 1}, "b": {"c": 2, "d": 3}}` into
    `{"c": {"a": 0, "b": 2}, "d": {"a": 1, "b": 3}}`.
    """
    return {
        k1: {
            r1[0]: r1[2]
            for r1 in r0
        }
        for (k1, r0) in group_sorted(
            (
                (k0, k1, v1)
                for (k0, v0) in dictionary.items()
                for (k1, v1) in v0.items()
            ),
            key=itemgetter(1)
        )
    }

bench_name_speed = re.compile(r"([^/]+)/([^/]+)/([^/]+)")

def parse_bench_name(name):
    reMatch = bench_name_speed.fullmatch(name)
    return {
        "library":
            "accuparsec" if reMatch[2] == "accu" else
            "attoparsec" if reMatch[2] == "atto" else
            error("unknown library"),
        "input": int(reMatch[3]),
        "grammar": reMatch[1]
    }

def unmarshal_speed(benchmarks):
    """
    unmarshals to the following shape.

    {prog500: {"error accumulation": 0.38561290569478285,
           "megaparsec": 0.4973198441612616},
     prog1000: {"error accumulation": 0.6617164389311493,
            "megaparsec": 0.7733381483842295},
     prog2000: {"error accumulation": 1.2625243996074444,
            "megaparsec": 1.367987776735256},
     prog4000: {"error accumulation": 2.4297940610620854,
            "megaparsec": 2.532344783954995},
      ...}
    """
    return {
        g: {
            i: {
                data2["library"]: data2["value"] for data2 in data1
            }
            for (i, data1) in group_sorted(data0, key=itemgetter("input"))
        }
        for (g, data0) in groupby(
            (
                {**parse_bench_name(c["reportName"]), "value": c["reportAnalysis"]["anRegress"][0]["regCoeffs"]["iters"]["estPoint"] * 10**3}
                for c in benchmarks
            ),
            key=itemgetter("grammar")
        )
    }

def plot(x_label, y_label, size, benchmarks, x_label_usetex=False, xtick_usetex=False, rotation=None, legend_column_count=None):
    """
    plots data of the following shape.
    
    {500: {"error accumulation": 0.38561290569478285,
           "megaparsec": 0.4973198441612616},
     1000: {"error accumulation": 0.6617164389311493,
            "megaparsec": 0.7733381483842295},
     2000: {"error accumulation": 1.2625243996074444,
            "megaparsec": 1.367987776735256},
     4000: {"error accumulation": 2.4297940610620854,
            "megaparsec": 2.532344783954995}}
    """
    tick_labels = benchmarks.keys()
    benchmarks = transpose_dictionaries(benchmarks)
    (fig, ax) = plt.subplots(layout="constrained", figsize=size, dpi=150)
    library_count = len(benchmarks)
    width = 1 / (library_count+1)
    for (library_index, (library, data)) in enumerate(sorted(benchmarks.items(), key=itemgetter(0))):
        x = np.arange(len(data))
        ax.bar(x + library_index * width, data.values(), width, label=library)
    ax.set_ylabel(y_label)
    ax.set_xlabel(x_label, usetex=x_label_usetex)
    ax.set_xticks(x + (library_count-1) * width / 2 , tick_labels, usetex=xtick_usetex, rotation=rotation)
    ax.legend(loc="upper left", ncol=legend_column_count if legend_column_count is not None else library_count)
    return fig

with open(sys.argv[1]) as f:
    speed = unmarshal_speed(j.load(f)[2])

# plot(
#     "input program",
#     "time [ms]",
#     (3.2, 4.8),
#     {
#         f"\\texttt{{{input}.gcl}}": value
#         for (input, value) in speed.items()
#         if input < 30
#     },
#     xtick_usetex=True,
#     rotation=45,
#     legend_column_count=1,
# ).savefig("bench_speed_gcl_fast.png")

plot(
    "input program",
    "time [ms]",
    (6.4, 4.8),
    {
        f"\\texttt{{{input}.gcl}}": value
        for (input, value) in speed["gcl"].items()
        if 50 <= input
    },
    xtick_usetex=True,
    rotation=45,
).savefig("bench_speed_gcl_slow.png")

plot(
    "input file",
    "time [ms]",
    (6.4, 4.8),
    {
        f"\\texttt{{{input}.json}}": value
        for (input, value) in speed["json"].items()
        if 50 <= input
    },
    xtick_usetex=True,
    rotation=45,
).savefig("bench_speed_json_slow.png")

# plt.show()

# def plot_speed_and_save(name, x_label, size, x_label_usetex=False):
#     return plot(
#         x_label,
#         "time in milliseconds",
#         size,
#         speed[name],
#         x_label_usetex
#     ).savefig(f"bench_{name}.png")
