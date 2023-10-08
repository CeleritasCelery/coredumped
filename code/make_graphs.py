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
        'buffer': 1.22,
        'crop': 3.6,
        'ropey': 11.0,
        'jumprope': 90.4,
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
        'buffer': 2.63,
        'crop': 70.1,
        'jumprope': 365.7,
        'ropey': 80.7,
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
    plt.text(2, 90, '%.1f%%' % data['jumprope'], ha='center',  fontsize=20)
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
    values_ns = [15.6, 428.1, 18.13e3, 642e3, 22.5e6]
    plt.semilogy(keys, values_ns)
    plt.text(keys[-1], values_ns[-1], time_formatter(values_ns[-1], None), ha='center')
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
    values = [211.87, 2.4718e3, 65.234e3, 6.4077e6, 251.41e6]
    create = [129.65, 1.1374e3, 36.912e3, 3.5040e6, 145.27e6]

    total = np.array(values) - np.array(create)
    plt.semilogy(keys, total)
    formatter_y = FuncFormatter(time_formatter)
    formatter_x = FuncFormatter(size_formatter)
    plt.gca().yaxis.set_major_formatter(formatter_y)
    plt.text(keys[-1], total[-1], time_formatter(total[-1], None), ha='center')
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
    formatter_y = FuncFormatter(time_formatter_1p)
    plt.rcParams["figure.figsize"] = (8,4)

    fig, subs = plt.subplots(ncols=2)
    key = "automerge"
    value = data[key]
    subs[0].bar(value.keys(), value.values())
    subs[0].bar(value.keys(), value.values(), color=['b', 'y', 'g', 'r'])
    subs[0].set_title(key)
    subs[0].yaxis.set_major_formatter(formatter_y)

    key = "rustcode"
    value = data[key]
    subs[1].bar(value.keys(), value.values())
    subs[1].bar(value.keys(), value.values(), color=['b', 'y', 'g', 'r'])
    subs[1].set_title(key)
    subs[1].yaxis.set_major_formatter(formatter_y)

    plt.subplots_adjust(wspace=0.3)

    plt.savefig('../static/images/realworld1.png')

    if SHOW_PLOT:
        plt.show()
    # plt.clf()

    _, subs = plt.subplots(ncols=2)
    key = "sveltecomponent"
    value = data[key]
    subs[0].bar(value.keys(), value.values())
    subs[0].bar(value.keys(), value.values(), color=['b', 'y', 'g', 'r'])
    subs[0].set_title(key)
    subs[0].yaxis.set_major_formatter(formatter_y)

    key = "seph-blog1"
    value = data[key]
    subs[1].bar(value.keys(), value.values())
    subs[1].bar(value.keys(), value.values(), color=['b', 'y', 'g', 'r'])
    subs[1].set_title(key)
    subs[1].yaxis.set_major_formatter(formatter_y)

    plt.subplots_adjust(wspace=0.3, hspace=0.3)
    plt.savefig('../static/images/realworld2.png')

    if SHOW_PLOT:
        plt.show()
    # plt.clf()

    _, subs = plt.subplots(ncols=2)
    key = "friendsforever"
    value = data[key]
    subs[0].bar(value.keys(), value.values())
    subs[0].bar(value.keys(), value.values(), color=['b', 'y', 'g', 'r'])
    subs[0].set_title(key)
    subs[0].yaxis.set_major_formatter(formatter_y)

    subs[1].remove()

    plt.subplots_adjust(wspace=0.3, hspace=0.3)
    plt.savefig('../static/images/realworld3.png')

    if SHOW_PLOT:
        plt.show()
    plt.clf()


def report_append():
    data = {
        'buffer': 131.8,
        'crop': 225.3,
        'jumprope': 808.7,
        'ropey': 779.1,
    }


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
    if SHOW_PLOT:
        plt.show()
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


def report_cursor_count():
    buffer = [3.2550e3, 31.742e3, 80.447e3, 163.29e3, 359.67e3, 740.61e3, 1.9475e6, 2.8052e6, 4.1142e6]
    crop = [4.9751e3, 56.685e3, 154.84e3, 316.62e3, 638.29e3, 1.2668e6, 3.1005e6, 4.3250e6, 6.1946e6]
    jumprope = [3.7554e3, 47.612e3, 134.79e3, 289.29e3, 602.58e3, 1.2237e6, 3.1034e6, 4.4057e6, 6.4385e6]
    ropey = [9.5603e3, 139.37e3, 349.06e3, 704.12e3, 1.4088e6, 2.7899e6, 6.9205e6, 9.4853e6, 13.231e6]
    count = [10, 100, 250, 500, 1000, 2000, 5000, 7000, 10000]
    # plot everything as a function of count
    formatter_y = FuncFormatter(time_formatter)
    plt.plot(count, buffer, label='buffer')
    plt.plot(count, crop, label='crop')
    plt.plot(count, jumprope, label='jumprope')
    plt.plot(count, ropey, label='ropey')
    plt.gca().yaxis.set_major_formatter(formatter_y)
    plt.xlabel('cursor count')
    plt.ylabel('time')
    plt.legend()
    plt.savefig('../static/images/cursor_count.png')
    if SHOW_PLOT:
        plt.show()


def report_cursor_distance():
    buffer = [29.813e3, 31.162e3, 32.722e3, 39.129e3, 44.745e3, 47.369e3, 55.927e3, 63.854e3, 80.070e3, 93.982e3, 106.09e3, 135.85e3, 152.83e3, 177.20e3, 198.60e3, 212.99e3]
    crop = [42.163e3, 56.570e3, 69.711e3, 72.096e3, 66.456e3, 59.695e3, 55.133e3, 51.822e3, 50.629e3, 51.458e3, 52.825e3, 54.583e3, 55.148e3, 55.450e3, 56.270e3, 58.363e3]
    jumprope = [39.153e3, 47.440e3, 44.820e3, 46.508e3, 48.359e3, 50.597e3, 51.534e3, 52.648e3, 54.249e3, 56.147e3, 57.499e3, 59.465e3, 60.277e3, 60.763e3, 61.921e3, 64.173e3]
    ropey = [123.19e3, 136.92e3, 139.29e3, 140.90e3, 142.66e3, 143.12e3, 144.75e3, 145.31e3, 146.71e3, 150.16e3, 148.60e3, 150.62e3, 152.24e3, 152.30e3, 151.16e3, 154.46e3]
    count = [10, 100, 250, 500, 750, 1000, 1250, 1500, 2000, 2500, 3000, 4000, 4250, 4500, 5000, 6000]
    # plot everything as a function of count
    formatter_y = FuncFormatter(time_formatter)
    plt.plot(count, buffer, label='buffer')
    plt.plot(count, crop, label='crop')
    plt.plot(count, jumprope, label='jumprope')
    plt.plot(count, ropey, label='ropey')
    plt.gca().yaxis.set_major_formatter(formatter_y)
    plt.xlabel('cursor distance (bytes)')
    plt.ylabel('time')
    plt.legend()
    plt.savefig('../static/images/cursor_distance.png')
    if SHOW_PLOT:
        plt.show()


def report_search_full():
    buffer =   [0, 31.309e3, 4.3073e6, 8.6157e6, 12.960e6, 17.228e6, 21.688e6, 25.933e6, 30.285e6, 34.488e6]
    crop =     [0, 72.753e3, 31.477e6, 68.626e6, 114.99e6, 132.51e6, 134.82e6, 164.66e6, 232.65e6, 253.14e6]
    jumprope = [0, 138.55e3, 41.465e6, 81.055e6, 110.87e6, 120.40e6, 156.95e6, 166.65e6, 206.09e6, 254.81e6]
    ropey =    [0, 86.966e3, 42.887e6, 66.483e6, 100.26e6, 118.14e6, 148.11e6, 186.52e6, 225.03e6, 241.91e6]
    move_gap = [0, 8.2886e3, 1.3293e6, 2.6857e6, 4.0778e6, 5.4573e6, 7.0241e6, 8.7642e6, 10.221e6, 11.841e6]
    distance = [0, 1048576, 134217728, 268435456, 402653184, 536870912, 671088640, 805306368, 939524096, 1073741824]

    formatter_y = FuncFormatter(time_formatter)
    formatter_x = FuncFormatter(size_formatter)
    plt.plot(distance, buffer, label='buffer')
    plt.plot(distance, crop, label='crop')
    plt.plot(distance, jumprope, label='jumprope')
    plt.plot(distance, ropey, label='ropey')


    plt.gca().yaxis.set_major_formatter(formatter_y)
    plt.gca().xaxis.set_major_formatter(formatter_x)
    plt.xticks([2**20, 2**28, 2*2**28, 3*2**28, 2**30])
    plt.xlabel('text size')
    plt.ylabel('time')
    plt.legend()
    plt.savefig('../static/images/search.png')
    if SHOW_PLOT:
        plt.show()

    plt.clf()
    plt.gca().yaxis.set_major_formatter(formatter_y)
    plt.gca().xaxis.set_major_formatter(formatter_x)
    plt.xticks([2**20, 2**28, 2*2**28, 3*2**28, 2**30])
    plt.plot(distance, buffer, label='buffer')
    plt.plot(distance, crop, label='crop')
    plt.plot(distance, jumprope, label='jumprope')
    plt.plot(distance, ropey, label='ropey')
    # add move_gap to buffer and plot the result
    buffer = np.array(buffer) + np.array(move_gap)
    plt.plot(distance, buffer, label='buffer + move gap')
    plt.xlabel('text size')
    plt.ylabel('time')
    plt.legend()
    plt.savefig('../static/images/search_move.png')
    if SHOW_PLOT:
        plt.show()



report_overhead()
report_edit_overhead()
# report_move_gap()
# report_resize()
# report_from_string()
# report_from_str()
# report_save()
# report_realworld()
# report_smart_diff()
# report_cursor_count()
# report_cursor_distance()
# report_search_full()
