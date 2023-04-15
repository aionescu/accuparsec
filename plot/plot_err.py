import matplotlib.pyplot as plt
import numpy as np

accu = [123, 123, 123]
atto = [123, 12, 131]

lengths = ["24", "43", "1111"]
width = 1 / len(accu)
 
(fig, ax) = plt.subplots(layout="constrained", figsize=(6.4, 4.8), dpi=150)

indices = np.arange(len(lengths))

ax.bar(indices - width/2, atto, width, label='Attoparsec')
ax.bar(indices + width/2, accu, width, label='Accuparsec')

ax.set_xticks(indices)
ax.set_xticklabels(lengths)

ax.set_ylabel("Average distance from error")
ax.set_xlabel("Input size", usetex=False)

fig.savefig("err.png")
# fig.show()