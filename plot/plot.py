import numpy as np
import matplotlib.pyplot as plt
import json as j
import re
from itertools import groupby
from operator import itemgetter
import sys
from decimal import Decimal

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

bench_name_speed = re.compile(r"([^/]+)/([^/]+)/([^ ]+)( .+)")

def parse_bench_name(name):
    reMatch = bench_name_speed.fullmatch(name)
    match reMatch[4]:
        case " B":
            unit = 1
        case " KiB":
            unit = 1024
        case " MiB":
            unit = 1024 * 1024
    return {
        "library":
            "accuparsec" if reMatch[2] == "accu" else
            "attoparsec" if reMatch[2] == "atto" else
            error("unknown library"),
        "input": (Decimal(reMatch[3]) * unit, f"{reMatch[3]}{reMatch[4]}"),
        "grammar": reMatch[1]
    }

def unmarshal_speed(benchmarks):
    """
    unmarshals to the following shape.

    {'gcl': {10: {'accuparsec': 0.7020476904880393,
                  'attoparsec': 0.9104941414657346},
             30: {'accuparsec': 20.405967427274373,
                  'attoparsec': 21.091928654553964},
             ...},
     'json': {10: {'accuparsec': 0.15545793422283005,
                   'attoparsec': 0.23267878032048916},
              ...}}
    """
    return {
        g: {
            i: {
                data2["library"]: data2["value"] for data2 in data1
            }
            for (i, data1) in group_sorted(data0, key=itemgetter("input"))
        }
        for (g, data0) in group_sorted(
            (
                {**parse_bench_name(c["reportName"]), "value": c["reportAnalysis"]["anRegress"][0]["regCoeffs"]["iters"]["estPoint"] * 10**3}
                for c in benchmarks
            ),
            key=itemgetter("grammar")
        )
    }

def plot(ax, x_label, y_label, benchmarks, x_label_usetex=False, xtick_usetex=False, rotation=None, legend=True):
    """
    plots data of the following shape.
    
    {10: {'accuparsec': 0.7020476904880393,
          'attoparsec': 0.9104941414657346},
     30: {'accuparsec': 20.405967427274373,
          'attoparsec': 21.091928654553964},
     ...}
    """
    tick_labels = benchmarks.keys()
    benchmarks = transpose_dictionaries(benchmarks)
    library_count = len(benchmarks)
    width = 1 / (library_count+1)
    for (library_index, (library, data)) in enumerate(sorted(benchmarks.items(), key=itemgetter(0))):
        x = np.arange(len(data))
        ax.bar(x + library_index * width, data.values(), width, label=library)
    ax.set_ylabel(y_label)
    ax.set_xlabel(x_label, usetex=x_label_usetex)
    ax.set_xticks(x + (library_count-1) * width / 2 , tick_labels, usetex=xtick_usetex, rotation=rotation)
    if legend:
        ax.legend(loc="upper left", ncol=library_count)

with open(sys.argv[1]) as f:
    speed = unmarshal_speed(j.load(f)[2])

(fig, ax) = plt.subplots(layout="constrained", figsize=(6.4, 4.8), dpi=150)
plot(
    ax,
    "input program",
    "time [ms]",
    {
        input[1]: value
        for (input, value) in speed["gcl"].items()
        if Decimal("483.20") * 1024 <= input[0]
    },
    rotation=45,
)
fig.savefig("bench_speed_gcl_slow.png")

(fig, ax) = plt.subplots(layout="constrained", figsize=(6.4, 4.8), dpi=150)
plot(
    ax,
    "input file",
    "time [ms]",
    {
        input[1]: value
        for (input, value) in speed["json"].items()
        if Decimal("621.71") * 1024 <= input[0]
    },
    rotation=45,
)
fig.savefig("bench_speed_json_slow.png")

# plt.show()
