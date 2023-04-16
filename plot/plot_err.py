import matplotlib.pyplot as plt
import numpy as np

# k = 5
# Input length
# 363
# 1349
# 90186
# "progs/err-eval-medium.gcl"
# Input length
# 3658
# 14262
# 105772
# "progs/err-eval-longest.gcl"
# Input length
# 17661
# 90567
# 151389
# accu = [1.35, 14.26, 90.57] 
# atto = [90.19, 105.77, 151.39]

# k = 1
# "progs/err-eval.gcl"
# Input length
# 363
# 2184
# 90186
# "progs/err-eval-medium.gcl"
# Input length
# 3658
# 15175
# 105772
# "progs/err-eval-longest.gcl"
# Input length
# 17661
# 91678
# 151389
accu = [2.18, 15.18, 91.68]
atto = [90.19, 105.77, 151.39]


lengths = ["363", "3658", "17661"]
width = 1 / len(accu)

(fig, ax) = plt.subplots(layout="constrained", figsize=(6.4, 4.8), dpi=150)

indices = np.arange(len(lengths))

ax.bar(indices - width/2, accu, width, label='accuparsec')
ax.bar(indices + width/2, atto, width, label='attoparsec')

ax.set_xticks(indices)
ax.set_xticklabels(lengths)

ax.set_ylabel("Average distance from error")
ax.set_xlabel("Input size (characters)", usetex=False)

fig.savefig("plot/err.png")
# fig.show()
