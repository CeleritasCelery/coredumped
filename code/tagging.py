#!/Users/troyhinckley/blog/code/code-env/bin/python
import numpy as np
from warnings import simplefilter
import matplotlib.pyplot as plt
from matplotlib.ticker import FuncFormatter
import seaborn
seaborn.set()

SHOW_PLOT = False

timings = {
    'sum_t0': {
        "baseline":  4.0676,
        "low_bits":  3.1183,
        "low_byte":  4.4915,
        "high_bits": 4.9314,
        "high_byte": 4.9291,
        "nan_box":   4.9970,
    },
    'sum_t1': {
        "baseline":  4.0604,
        "low_bits":  4.9604,
        "low_byte":  4.9549,
        "high_bits": 4.9611,
        "high_byte": 4.9521,
        "nan_box":   5.1323,
    },
    'sum_t1_x86': {
        "baseline":  3.7384,
        "low_bits":  3.5906,
        "low_byte":  2.8909,
        "high_bits": 3.2281,
        "high_byte": 3.6009,
        "nan_box":   3.6001,
    },
    'sum_t7_x86': {
        "baseline":  2.4956,
        "low_bits":  2.4060,
        "low_byte":  1.9355,
        "high_bits": 2.7483,
        "high_byte": 2.6981,
        "nan_box":   2.4112,
    },
    'count_t0': {
        "baseline":  2.7881,
        "low_bits":  1.1700,
        "low_byte":  1.1695,
        "high_bits": 0.7811,
        "high_byte": 0.7812,
        "nan_box":   1.1696,
    },
    'count_t1': {
        "baseline":  2.8011,
        "low_bits":  1.1695,
        "low_byte":  1.1695,
        "high_bits": 1.1697,
        "high_byte": 1.1697,
        "nan_box":   1.1699,
    },
    'count_t7': {
        "baseline":  2.7918,
        "low_bits":  1.1756,
        "low_byte":  1.1700,
        "high_bits": 0.7811,
        "high_byte": 1.1697,
        "nan_box":   1.1696,
    },
    'sum_t0_t1': {
        "baseline":  4.0399,
        "low_bits":  4.4996,
        "low_byte":  4.4992,
        "high_bits": 4.9336,
        "high_byte": 4.9255,
        "nan_box":   4.9918,
    },
    'sum_t0_t2': {
        "baseline":  4.4677,
        "low_bits":  5.0900,
        "low_byte":  5.0881,
        "high_bits": 5.0906,
        "high_byte": 5.0880,
        "nan_box":   5.7282,
    },
    'sum_t0_t2_x86': {
        "baseline":  2.2621,
        "low_bits":  2.2686,
        "low_byte":  2.3431,
        "high_bits": 2.7148,
        "high_byte": 2.8573,
        "nan_box":   4.0631,
    },
    'sum_t0_t2_offset': {
        "baseline":  4.1518,
        "low_bits":  4.3073,
        "low_byte":  4.4628,
        "high_bits": 5.3541,
        "high_byte": 5.3912,
        "nan_box":   5.4191,
    },
    'count_t0_t1': {
        "baseline":  2.7841,
        "low_bits":  1.1679,
        "low_byte":  1.1734,
        "high_bits": 0.7803,
        "high_byte": 0.7804,
        "nan_box":   1.1682,
    },
    'count_t0_t2': {
        "baseline":  2.7849,
        "low_bits":  1.2258,
        "low_byte":  1.2209,
        "high_bits": 1.1694,
        "high_byte": 1.1690,
        "nan_box":   1.1689,
    },
    'count_t1_t2': {
        "baseline":  2.7916,
        "low_bits":  1.7513,
        "low_byte":  1.5562,
        "high_bits": 1.9448,
        "high_byte": 1.9437,
        "nan_box":   3.1100,
    },
    'sum_t1_t3_t5': {
        "baseline":  5.2799,
        "low_bits":  6.3279,
        "low_byte":  6.3176,
        "high_bits": 6.3189,
        "high_byte": 6.3210,
        "nan_box":   7.2040,
    },
    'sum_t1_t3_t5_x86': {
        "baseline":  3.6283,
        "low_bits":  4.6642,
        "low_byte":  3.6114,
        "high_bits": 3.6325,
        "high_byte": 3.9978,
        "nan_box":   5.9646,
    },
    'count_t1_t2_x86': {
        "baseline":  3.3015,
        "low_bits":  3.6023,
        "low_byte":  3.2983,
        "high_bits": 3.6614,
        "high_byte": 3.6033,
        "nan_box":   4.6563,
    },
    'count_t0_t1_t2': {
        "baseline":  2.7939,
        "low_bits":  1.1726,
        "low_byte":  1.3653,
        "high_bits": 1.5595,
        "high_byte": 1.5603,
        "nan_box":   2.3390,
    },
    'count_t0_t2_t4': {
        "baseline":  4.5646,
        "low_bits":  5.5271,
        "low_byte":  4.2787,
        "high_bits": 6.2670,
        "high_byte": 4.2781,
        "nan_box":   6.6093,
    },
    'count_t1_t2_t3': {
        "baseline":  2.7981,
        "low_bits":  1.7564,
        "low_byte":  1.5596,
        "high_bits": 1.9472,
        "high_byte": 1.9478,
        "nan_box":   3.1126,
    },
    'count_t1_t2_t3_x86': {
        "baseline":  3.2508,
        "low_bits":  3.6024,
        "low_byte":  3.2353,
        "high_bits": 3.6002,
        "high_byte": 3.5965,
        "nan_box":   4.6743,
    },
    'count_t1_t3_t5_x86': {
        "baseline":  3.6283,
        "low_bits":  4.6642,
        "low_byte":  3.6114,
        "high_bits": 3.6325,
        "high_byte": 3.9978,
        "nan_box":   5.9646,
    },
    'call7': {
        "baseline":  9.3727,
        "low_bits":  9.3284,
        "low_byte":  9.3219,
        "high_bits": 9.3296,
        "high_byte": 9.3246,
        "nan_box":   9.3254,
    },
    'call8': {
        "baseline":  12.476,
        "low_bits":  9.3270,
        "low_byte":  9.3229,
        "high_bits": 9.3104,
        "high_byte": 9.3150,
        "nan_box":   9.3115,
    },
    'chunks': {
        "baseline":  31.955,
        "low_bits":  24.916,
        "low_byte":  24.894,
        "high_bits": 24.981,
        "high_byte": 24.943,
        "nan_box":   25.174,
    },
    'elided': {
        "baseline":  1.8179,
        "low_bits":  3.9087,
        "low_byte":  2.3494,
        "high_bits": 2.3485,
        "high_byte": 4.9224,
        "nan_box":   3.8999,
    },
    'elided_hint': {
        "baseline":  1.8070,
        "low_bits":  1.8071,
        "low_byte":  1.8059,
        "high_bits": 1.8075,
        "high_byte": 4.5664,
        "nan_box":   1.8073,
    },
}


def plot_graph(key, sub):
    data = timings[key]
    # for the data keys, remove underscores
    data = {k.replace('_', ' '): v for k, v in data.items()}
    # plot the data as a bar graph
    sub.bar(data.keys(), data.values())
    sub.set_ylabel('Time (us)')
    # make each column a different color
    for i, bar in enumerate(sub.patches):
        bar.set_color(plt.cm.viridis(i/len(data)))


def plot_data(key, title):
    _, sub = plt.subplots()
    plot_graph(key, sub)
    sub.set_title(title)
    plt.savefig(f"../static/images/tagging/{key}.png")

    if SHOW_PLOT:
        plt.show()


# create a function that takes two keys and plots them both graphs in the same figure
def plot_data_2(key1, title1, key2, title2):
    _, sub = plt.subplots(ncols=2)
    plot_graph(key1, sub[0])
    plot_graph(key2, sub[1])
    sub[0].set_title(title1)
    sub[1].set_title(title2)
    # make the plot wider
    plt.gcf().set_size_inches(12, 6)

    plt.savefig(f"../static/images/tagging/{key1}_{key2}.png")

    if SHOW_PLOT:
        plt.show()

plot_data("sum_t0", "Sum Tag0")
plot_data("sum_t1", "Sum Tag1")
plot_data("sum_t1_x86", "Sum Tag1 x86")
plot_data_2("sum_t1", "Sum Tag1 ARM", "sum_t1_x86", "Sum Tag1 x86")
plot_data("sum_t7_x86", "Sum Tag7 x86")
plot_data("count_t0", "Count Tag0")
plot_data("count_t1", "Count Tag1")
plot_data("count_t7", "Count Tag7")
plot_data("sum_t0_t1", "Sum Tag(0,1)")
plot_data("sum_t0_t2", "Sum Tag(0,2)")
plot_data("sum_t0_t2_x86", "Sum Tag(0,2) x86")
plot_data("count_t0_t1", "Count Tag(0,1)")
plot_data("count_t0_t2", "Count Tag(0,2)")
plot_data_2("count_t0_t1_t2", "Count Contiguous Tags(0-2)", "count_t0_t2_t4", "Count Non-Contiguous Tags(0,2,4)")
plot_data("count_t1_t2", "Count Tag(1,2)")
plot_data("count_t1_t2_x86", "Count Tag(1,2) x86")
plot_data_2("count_t1_t2", "Count Tag(1,2) ARM", "count_t1_t2_x86", "Count Tag(1,2) x86")
plot_data_2("sum_t1_t3_t5", "Count Tag(1,3,5) ARM", "count_t1_t3_t5_x86", "Count Tag(1,3,5) x86")
plot_data("count_t0_t1_t2", "Count Tag(0-2)")
plot_data("count_t0_t2_t4", "Count Tag(0,2,4)")
plot_data("count_t1_t2_t3", "Count Tag(1-3)")
plot_data("count_t1_t2_t3_x86", "Count Tag(1-3) x86")
plot_data_2("count_t1_t2_t3", "Count Tag(1-3) ARM", "count_t1_t2_t3_x86", "Count Tag(1-3) x86")
plot_data("count_t1_t3_t5_x86", "Count Tag(1,3,5) x86")
plot_data_2("call7", "Call with 7 args", "call8", "Call with 8 args")
plot_data("chunks", "Chunks")
plot_data("elided", "Elided")
plot_data("elided_hint", "Elided with Hint")
