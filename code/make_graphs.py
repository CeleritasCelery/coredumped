#!/usr/bin/env python3
import numpy as np
from warnings import simplefilter
import matplotlib.pyplot as plt
from matplotlib.ticker import FuncFormatter
import seaborn as sns
sns.set()


SHOW_PLOT = True

def time_formatter_1p(x, _):
    if x == 0:
        return ''
    if x < 1e3:
        return f'{x:.1f}ns'
    elif x < 1e6:
        return f'{x / 1e3:.1f}µs'
    else:
        return f'{x / 1e6:.1f}ms'

def time_formatter(x, _):
    if x == 0:
        return ''
    if x < 1e3:
        return f'{x:.0f}ns'
    elif x < 1e6:
        return f'{x / 1e3:.0f}µs'
    else:
        return f'{x / 1e6:.0f}ms'

# size_formatter function based on powers of two
def size_formatter(num, _):
    for unit in ['B', 'KB', 'MB', 'GB', 'TB']:
        if abs(num) < 1024.0:
            return "%3.0f%s" % (num, unit)
        num /= 1024.0
    return "%.1f%s" % (num, 'YB')


def report_overhead():
    data = {
        'buffer': 1.1,
        'crop': 3.6,
        'jumprope': 90.4,
        'ropey': 11.0,
    }
    # make a bar graph of the data
    plt.bar(data.keys(), data.values())
    plt.xlabel('Container')
    plt.ylabel('overhead')
    plt.bar(data.keys(), data.values(), color=['b', 'y', 'g', 'r'])
    # label the bar heights
    for x, y in enumerate(data.values()):
        plt.text(x, y + 0.5, '%.1f%%' % y, ha='center')
    # set the y axis range from 0 to 100
    plt.ylim(0, 100)
    # save the plot as a png
    plt.title('overhead')
    plt.savefig('../static/images/buffer_overhead.png')
    if SHOW_PLOT:
        plt.show()

def report_edit_overhead():
    data = {
        'buffer': 1.9,
        'crop': 70.1,
        'ropey': 80.7,
        'jumprope': 365.7,
    }
    # make a bar graph of the data
    plt.bar(data.keys(), data.values())
    plt.bar(data.keys(), data.values(), color=['b', 'y', 'g', 'r'])
    plt.xlabel('Container')
    plt.ylabel('overhead')
    plt.ylim(0, 100)
    # label the bar heights
    for x, y in enumerate(data.values()):
        plt.text(x, y + 0.5, '%.1f%%' % y, ha='center')
    # plot jumprope separately because it is so high
    # make the text bold
    plt.text(3, 90, '%.1f%%' % data['jumprope'], ha='center',  fontsize=20)
    # save the plot as a png
    plt.title('edit overhead')
    plt.savefig('../static/images/buffer_edit_overhead.png')
    if SHOW_PLOT:
        plt.show()


def report_from_string():
    clone = 107.4
    data = {
        'buffer': 147.0 - clone,
        'crop': 345.6 - clone,
        'jumprope': 970.1 - clone,
        'ropey': 830.5 - clone,
    }
    # make a bar graph of the data
    plt.bar(data.keys(), data.values())
    plt.bar(data.keys(), data.values(), color=['b', 'y', 'g', 'r'])
    plt.ylim(0, 1000)
    plt.xlabel('Container')
    plt.ylabel('time (ms)')
    plt.title('From<String>')
    plt.savefig('../static/images/from_string.png')
    if SHOW_PLOT:
        plt.show()

def report_from_str():
    data = {
        'buffer': 131.8,
        'crop': 225.3,
        'jumprope': 808.7,
        'ropey': 779.1,
    }
    # make a bar graph of the data
    plt.bar(data.keys(), data.values())
    plt.bar(data.keys(), data.values(), color=['b', 'y', 'g', 'r'])
    # set y limit to 1000
    plt.ylim(0, 1000)
    plt.xlabel('Container')
    plt.ylabel('time (ms)')
    plt.title('From<str>')
    plt.savefig('../static/images/from_str.png')
    if SHOW_PLOT:
        plt.show()

def report_save():
    data = {
        'buffer': 20,
        'crop': 39.3,
        'jumprope': 32.4,
        'ropey': 109.9,
    }
    # make a bar graph of the data
    plt.bar(data.keys(), data.values())
    plt.bar(data.keys(), data.values(), color=['b', 'y', 'g', 'r'])
    plt.xlabel('Container')
    plt.ylabel('time (μs)')
    plt.title('ToString')
    plt.savefig('../static/images/save.png')
    if SHOW_PLOT:
        plt.show()


def report_move_gap():
    keys = ['1KB', '32KB', '1MB', '32MB', '1GB']
    values_ns = [15.6, 430.4, 18.47e3, 652e3, 22.95e6]
    plt.semilogy(keys, values_ns)
    formatter_y = FuncFormatter(time_formatter)
    plt.gca().yaxis.set_major_formatter(formatter_y)
    plt.xlabel('size')
    plt.ylabel('time')
    plt.title('move gap')
    # save the plot as a png
    plt.savefig('../static/images/buffer_move_gap.png')
    if SHOW_PLOT:
        plt.show()


def report_resize():
    keys = [2**10, 2**15, 2**20, 2**25, 2**30]
    values_ns = [103.4, 1.45e3, 53.2e3, 3.79e6, 124.58e6]
    plt.semilogy(keys, values_ns)
    formatter_y = FuncFormatter(time_formatter)
    formatter_x = FuncFormatter(size_formatter)
    plt.gca().yaxis.set_major_formatter(formatter_y)
    plt.text(keys[-1], values_ns[-1], time_formatter(values_ns[-1], None), ha='center')
    # make the x axis logarithmic
    plt.xscale('log', base=2)
    plt.gca().xaxis.set_major_formatter(formatter_x)

    plt.xlabel('size')
    plt.ylabel('time')
    plt.title('resize')
    # save the plot as a png
    plt.savefig('../static/images/buffer_resize.png')
    if SHOW_PLOT:
        plt.show()

def report_realworld():
    data = {
        "automerge": {
            'buffer': 9.7e6,
            'crop': 11e6,
            'jumprope': 10.8e6,
            'ropey': 35e6,
        },
        "rustcode": {
            'buffer': 1.7e6,
            'crop': 2e6,
            'jumprope': 1.9e6,
            'ropey': 6.3e6,
        },
        "sveltecomponent": {
            'buffer': 542.6e3,
            'crop': 634e3,
            'jumprope': 758.5e3,
            'ropey': 3.3e6,
        },
        "seph-blog1": {
            'buffer': 4e6,
            'crop': 4.9e6,
            'jumprope': 4.4e6,
            'ropey': 17.9e6,
        },
        "friendsforever": {
            'buffer': 228.6e3,
            'crop': 187.3e3,
            'jumprope': 231.9e3,
            'ropey': 765.1e3,
        },
    }
    # for each element in data create a bar chart. put all the charts in one figure
    _, subs = plt.subplots(nrows=2, ncols=3)

    formatter_y = FuncFormatter(time_formatter_1p)
    for i, (key, value) in enumerate(data.items()):
        x = i % 2
        y = i % 3
        subs[x, y].bar(value.keys(), value.values())
        subs[x, y].bar(value.keys(), value.values(), color=['b', 'y', 'g', 'r'])
        subs[x, y].set_title(key)
        subs[x, y].yaxis.set_major_formatter(formatter_y)

    subs[1, 2].remove()

    plt.savefig('../static/images/realworld.png')
    if SHOW_PLOT:
        plt.show()

def report_smart_diff():
    naive = [367.70e3, 408.52e3, 414.83e3, 494.08e3, 658.56e3, 957.86e3, 1.3638e6, 1.7848e6, 2.2606e6, 2.7199e6, 3.1930e6, 3.6569e6, 4.0212e6, 4.5252e6, 4.9372e6]
    smart = [364.50e3, 395.56e3, 406.54e3, 458.55e3, 576.12e3, 748.48e3, 994.40e3, 1.2723e6, 1.7882e6, 2.3022e6, 2.6341e6, 2.9289e6, 3.1035e6, 3.3365e6, 3.5993e6]
    distance = [10e3, 50e3, 100e3, 250e3, 500e3, 1e6, 2e6, 3e6, 4e6, 5e6, 6e6, 7e6, 8e6, 9e6, 10e6]
    # plot naive and smart as lines graphs as a function of distance

    formatter_y = FuncFormatter(time_formatter)
    formatter_x = FuncFormatter(size_formatter)
    plt.plot(distance, naive, label='naive')
    plt.gca().yaxis.set_major_formatter(formatter_y)
    plt.gca().xaxis.set_major_formatter(formatter_x)
    # shade the region between the two lines
    plt.fill_between(distance, naive, smart, alpha=0.2)
    plt.plot(distance, smart, label='smart')
    plt.xlabel('distance between first and last cursor')
    plt.ylabel('time')
    plt.title('Multiple cursors comparision')
    plt.legend()
    plt.savefig('../static/images/smart_diff.png')
    # if SHOW_PLOT:
    #     plt.show()
    plt.clf()

    # take the perecentage difference between the two lines and plot that
    percent_diff = np.abs(np.array(naive) - np.array(smart)) / np.array(naive) * 100
    plt.plot(distance, percent_diff)
    plt.gca().yaxis.set_major_formatter(FuncFormatter(lambda x, _: f'{x:.0f}%'))
    plt.gca().xaxis.set_major_formatter(formatter_x)
    plt.xlabel('distance between first and last cursor')
    plt.ylabel('percent difference')
    plt.ylim(0, 100)
    # shade the area under the line
    plt.fill_between(distance, percent_diff, alpha=0.2)
    # smooth the line
    plt.title('Multiple cursors diff')
    plt.savefig('../static/images/smart_diff_percent.png')
    if SHOW_PLOT:
        plt.show()




# report_overhead()
# report_edit_overhead()
# report_move_gap()
# report_resize()
# report_from_string()
# report_from_str()
# report_save()
# report_realworld()
report_smart_diff()
