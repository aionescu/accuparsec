import matplotlib.pyplot as plt

(fig, ax) = plt.subplots(layout="constrained", figsize=(6.4, 4.8), dpi=150)
library_count = len(benchmarks)
width = 1 / (library_count+1)
# for (library_index, (library, data)) in enumerate(sorted(benchmarks.items(), key=itemgetter(0))):
#     x = np.arange(len(data))
#     ax.bar(x + library_index * width, data.values(), width, label=library)
ax.set_ylabel("input size")
ax.set_xlabel("avg distance from error", usetex=x_label_usetex)
ax.set_xticks(x + (library_count-1) * width / 2 , tick_labels, usetex=xtick_usetex, rotation=rotation)
# ax.legend(loc="upper left", ncol=legend_column_count if legend_column_count is not None else library_count)
fig.savefig("bench_speed_json_slow.png")