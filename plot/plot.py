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

bench_name_speed = re.compile(r"([^/]+)/([^/]+)/[^-]+-(err-)?([^.]+).*")

def parse_bench_name(name):
    reMatch = bench_name_speed.fullmatch(name)
    return {
        "library":
            "accuparsec" if reMatch[2] == "accu" else
            "attoparsec" if reMatch[2] == "atto" else
            error("unknown library"),
        "size": int(reMatch[4]),
        "errors": reMatch[3] is not None,
        "grammar": reMatch[1]
    }

def unmarshal_speed(benchmarks):
    """
    unmarshals to the following shape.

    ({'errors': False,
      'grammar': 'gcl',
      'size': 106833,
      'value': {'accuparsec': 0.01937118285489776,
                'attoparsec': 0.021256582474023906}},
     {'errors': True,
      'grammar': 'gcl',
      'size': 106833,
      'value': {'accuparsec': 0.019384692577976273,
                'attoparsec': 0.021690398030040147}},
     {'errors': False,
      'grammar': 'gcl',
      'size': 494793,
      'value': {'accuparsec': 0.09231922349987742,
                'attoparsec': 0.10275646000000042}},
     ...)
    """
    return (
        {"size": s, "errors": e, "grammar": g, "value": {data1["library"]: data1["value"] for data1 in data0}}
        for ((g, s, e), data0) in group_sorted(
            (
                {**parse_bench_name(c["reportName"]), "value": c["reportAnalysis"]["anRegress"][0]["regCoeffs"]["iters"]["estPoint"]}
                for c in benchmarks
            ),
            key=itemgetter("grammar", "size", "errors")
        )
    )

def plot(ax, x_label, y_label, benchmarks, x_label_usetex=False, xtick_usetex=False, rotation=None, legend=True):
    """
    plots data of the following shape.
    
    {'1.30 MiB': {'accuparsec': 246.10533009999924,
                  'attoparsec': 260.39305789981876},
     '1.30 MiB (Err)': {'accuparsec': 252.52285329970618,
                        'attoparsec': 263.5828087997652},
     '104.33 KiB': {'accuparsec': 19.37118285489776,
                    'attoparsec': 21.256582474023908},
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

def make_x_label(size, errors):
    (divisor, unit) = (
        (1, "B") if size < 1024 else
        (1024, "KiB") if size < 1024 * 1024 else
        (1024 * 1024, "MiB")
    )
    return f"{size/divisor:.2f} {unit}" + (" (Err)" if errors else "")

def to_milliseconds(value):
    return {k: v * 10**3 for (k, v) in value.items()}

with open(sys.argv[1]) as f:
    speed = tuple(unmarshal_speed(j.load(f)[2]))

(fig, ax) = plt.subplots(layout="constrained", figsize=(6.4, 4.8), dpi=150)
plot(
    ax,
    "input size",
    "time (ms)",
    {
        make_x_label(case["size"], case["errors"]): to_milliseconds(case["value"])
        for case in speed
        if case["grammar"] == "gcl"
    },
    rotation=45,
)
fig.savefig("bench_speed_gcl_slow.png")

(fig, ax) = plt.subplots(layout="constrained", figsize=(6.4, 4.8), dpi=150)
plot(
    ax,
    "input size",
    "time (ms)",
    {
        make_x_label(case["size"], case["errors"]): to_milliseconds(case["value"])
        for case in speed
        if case["grammar"] == "json"
    },
    rotation=45,
)
fig.savefig("bench_speed_json_slow.png")

# plt.show()
