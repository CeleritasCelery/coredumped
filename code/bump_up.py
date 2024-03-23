#!/usr/bin/env python3.11
import numpy as np
from warnings import simplefilter
import matplotlib.pyplot as plt
from matplotlib.ticker import FuncFormatter
import seaborn as sns
sns.set()



SHOW_PLOT = True


timings = {
    'alloc/big/byte/up': 9.9,
    'alloc/big/orig/down': 7.5,
    'alloc/big/orig/up': 9.3,
    'alloc/big/word/down': 7.5,
    'alloc/big/word/up': 9.9,
    'alloc/medium/byte/up': 9.9,
    'alloc/medium/orig/down': 7.5,
    'alloc/medium/orig/up': 9.3,
    'alloc/medium/word/down': 4.8,
    'alloc/medium/word/up': 3.6,
    'alloc/small/byte/up': 3.1,
    'alloc/small/orig/down': 4.7,
    'alloc/small/orig/up': 4.8,
    'alloc/small/word/down': 4.7,
    'alloc/small/word/up': 3.2,
    'alloc_slice/small/byte/up': 17.1,
    'alloc_slice/small/orig/down': 18.7,
    'alloc_slice/small/orig/up': 18.7,
    'alloc_slice/small/word/down': 18.7,
    'alloc_slice/small/word/up': 17.3,
    'alloc/small/2word/up': 3.2,
    'alloc/small/2word/down': 4.7,
    'alloc/medium/2word/up': 3.7,
    'alloc/medium/2word/down': 4.7,
    'alloc/big/2word/up': 3.5,
    'alloc/big/2word/down': 4.8,
}

baseline_u8 = timings['alloc/small/orig/down']
baseline_u64 = timings['alloc/medium/orig/down']
baseline_u128 = timings['alloc/big/orig/down']
baseline_slice = timings['alloc_slice/small/orig/down']


def report_orig():
    data = {
        'orig_bytes_up' : timings['alloc/small/orig/up'],
        'orig_bytes_down': timings['alloc/small/orig/down'],
        'orig_words_up' : timings['alloc/medium/orig/up'],
        'orig_words_down' : timings['alloc/medium/orig/down'],
        'orig_2words_up' : timings['alloc/big/orig/up'],
        'orig_2words_down' : timings['alloc/big/orig/down'],
        'orig_byte_slice_up': timings['alloc_slice/small/orig/up'],
        'orig_byte_slice_down': timings['alloc_slice/small/orig/down'],
    }
    # make a bar graph of the data, grouping the up and down times. Create two
    # plots in the same figure, one for the regular allocations and one for the
    # slice allocations
    fig, sub = plt.subplots()
    bar_width = 0.35
    index = np.arange(4)
    formatter = FuncFormatter(lambda x, _: f'{x:.0f}us')
    sub.yaxis.set_major_formatter(formatter)
    sub.bar(index, [data['orig_bytes_up'], data['orig_words_up'], data['orig_2words_up'], data['orig_byte_slice_up']], bar_width, label='Up', color='g')
    sub.bar(index + bar_width, [data['orig_bytes_down'], data['orig_words_down'], data['orig_2words_down'], data['orig_byte_slice_down']], bar_width, label='Down', color='r')
    sub.set_ylabel('Time')
    sub.set_xticks(index + (0.5*bar_width))
    sub.set_xticklabels(['u8', 'u64', 'u128', 'Slice'])
    sub.legend()

    # put a title at the top of the figure
    fig.suptitle('Original Functions')
    plt.savefig('../static/images/bump/original.png')

    if SHOW_PLOT:
        plt.show()


def report_v2():
    data = {
        'orig_bytes_up' : timings['alloc/small/orig/up']/baseline_u8,
        'min1_bytes_up' : timings['alloc/small/byte/up']/baseline_u8,
        'orig_words_up' : timings['alloc/medium/orig/up']/baseline_u64,
        'min1_words_up' : timings['alloc/medium/byte/up']/baseline_u64,
        'orig_2words_up' : timings['alloc/big/orig/up']/baseline_u128,
        'min1_2words_up' : timings['alloc/big/byte/up']/baseline_u128,
        'orig_byte_slice_up': timings['alloc_slice/small/orig/up']/baseline_slice,
        'min1_byte_slice_up': timings['alloc_slice/small/byte/up']/baseline_slice,
    }

    # make a bar graph of the data, grouping the up and down times. Create two
    # plots in the same figure, one for the regular allocations and one for the
    # slice allocations
    fig, sub = plt.subplots()
    bar_width = 0.35
    index = np.arange(4)
    # create a y axis formatter to show relative change from 1.0
    formatter = FuncFormatter(lambda x, _: f'{x:.2f}x')
    sub.yaxis.set_major_formatter(formatter)
    sub.bar(index, [data['orig_bytes_up'], data['orig_words_up'], data['orig_2words_up'], data['orig_byte_slice_up']], bar_width, label='Original', color='g')
    sub.bar(index + bar_width, [data['min1_bytes_up'], data['min1_words_up'], data['min1_2words_up'], data['min1_byte_slice_up']], bar_width, label='Single Branch', color='b')
    sub.set_ylabel('Time')
    sub.set_xticks(index + (0.5*bar_width))
    sub.set_xticklabels(['u8', 'u64', 'u128', 'Slice'])
    sub.legend()
    # draw a line at 1.0 to show the baseline
    sub.axhline(y=1.0, color='r', linestyle='--')


    # put a title at the top of the figure
    fig.suptitle('Bump Up')
    plt.savefig('../static/images/bump/up_v2.png')

    if SHOW_PLOT:
        plt.show()


def report_min8_up():
    data = {
        'min8_bytes_up' : timings['alloc/small/word/up']/baseline_u8,
        'min1_bytes_up' : timings['alloc/small/byte/up']/baseline_u8,
        'min8_words_up' : timings['alloc/medium/word/up']/baseline_u64,
        'min1_words_up' : timings['alloc/medium/byte/up']/baseline_u64,
        'min8_2words_up' : timings['alloc/big/word/up']/baseline_u128,
        'min1_2words_up' : timings['alloc/big/byte/up']/baseline_u128,
        'min8_byte_slice_up': timings['alloc_slice/small/word/up']/baseline_slice,
        'min1_byte_slice_up': timings['alloc_slice/small/byte/up']/baseline_slice,
    }
    # make a bar graph of the data, grouping the up and down times. Create two
    # plots in the same figure, one for the regular allocations and one for the
    # slice allocations
    fig, sub = plt.subplots()
    bar_width = 0.35
    index = np.arange(4)
    formatter = FuncFormatter(lambda x, _: f'{x:.2f}x')
    sub.yaxis.set_major_formatter(formatter)
    sub.bar(index , [data['min1_bytes_up'], data['min1_words_up'], data['min1_2words_up'], data['min1_byte_slice_up']], bar_width, label='align=1', color='b')
    sub.bar(index + bar_width, [data['min8_bytes_up'], data['min8_words_up'], data['min8_2words_up'], data['min8_byte_slice_up']], bar_width, label='align=8', color='teal')
    sub.set_ylabel('Time')
    sub.set_xticks(index + (0.5*bar_width))
    sub.set_xticklabels(['u8', 'u64', 'u128', 'Slice'])
    sub.legend()
    sub.axhline(y=1.0, color='r', linestyle='--')

    # put a title at the top of the figure
    fig.suptitle('Bump Up')
    plt.savefig('../static/images/bump/up_aligned.png')

    if SHOW_PLOT:
        plt.show()

def report_all():
    data = {
        'min8_bytes_up' : timings['alloc/small/word/up']/baseline_u8,
        'min1_bytes_up' : timings['alloc/small/byte/up']/baseline_u8,
        'min8_bytes_down': timings['alloc/small/word/down']/baseline_u8,
        'min8_words_up' : timings['alloc/medium/word/up']/baseline_u64,
        'min1_words_up' : timings['alloc/medium/byte/up']/baseline_u64,
        'min8_words_down' : timings['alloc/medium/word/down']/baseline_u64,
        'min8_2words_up' : timings['alloc/big/word/up']/baseline_u128,
        'min1_2words_up' : timings['alloc/big/byte/up']/baseline_u128,
        'min8_2words_down' : timings['alloc/big/word/down']/baseline_u128,
        'min8_byte_slice_up': timings['alloc_slice/small/word/up']/baseline_slice,
        'min1_byte_slice_up': timings['alloc_slice/small/byte/up']/baseline_slice,
        'min8_byte_slice_down': timings['alloc_slice/small/word/down']/baseline_slice,
    }
    # make a bar graph of the data, grouping the up and down times. Create two
    # plots in the same figure, one for the regular allocations and one for the
    # slice allocations
    fig, sub = plt.subplots()
    bar_width = 0.35
    index = np.arange(4)
    formatter = FuncFormatter(lambda x, _: f'{x:.2f}x')
    sub.yaxis.set_major_formatter(formatter)
    sub.bar(index + (1*bar_width), [data['min8_bytes_up'], data['min8_words_up'], data['min8_2words_up'], data['min8_byte_slice_up']], bar_width, label='Up(align=8)', color='teal')
    sub.bar(index + (2*bar_width), [data['min8_bytes_down'], data['min8_words_down'], data['min8_2words_down'], data['min8_byte_slice_down']], bar_width, label='Down(align=8)', color='orange')
    sub.set_ylabel('Time')
    sub.set_xticks(index + (1.5*bar_width))
    sub.set_xticklabels(['u8', 'u64', 'u128', 'Slice'])
    sub.legend()
    sub.axhline(y=1.0, color='r', linestyle='--')
    fig.suptitle('Bump Up/Down Aligned')
    plt.savefig('../static/images/bump/up_down_aligned.png')

    if SHOW_PLOT:
        plt.show()


def report_u128():
    data = {
        'min16_bytes_up' : timings['alloc/small/2word/up']/baseline_u8,
        'min16_bytes_down': timings['alloc/small/2word/down']/baseline_u8,
        'min16_words_up' : timings['alloc/medium/2word/up']/baseline_u64,
        'min16_words_down' : timings['alloc/medium/2word/down']/baseline_u64,
        'min16_2words_up' : timings['alloc/big/2word/up']/baseline_u128,
        'min16_2words_down' : timings['alloc/big/2word/down']/baseline_u128,
    }
    # make a bar graph of the data, grouping the up and down times. Create two
    # plots in the same figure, one for the regular allocations and one for the
    # slice allocations
    fig, sub = plt.subplots()
    bar_width = 0.35
    index = np.arange(3)
    formatter = FuncFormatter(lambda x, _: f'{x:.2f}x')
    sub.yaxis.set_major_formatter(formatter)
    sub.bar(index + (1*bar_width), [data['min16_bytes_up'], data['min16_words_up'], data['min16_2words_up']], bar_width, label='Up(align=16)', color='teal')
    sub.bar(index + (2*bar_width), [data['min16_bytes_down'], data['min16_words_down'], data['min16_2words_down'], ], bar_width, label='Down(align=16)', color='orange')
    sub.set_ylabel('Time')
    sub.set_xticks(index + (1.5*bar_width))
    sub.set_xticklabels(['u8', 'u64', 'u128'])
    sub.legend()
    sub.axhline(y=1.0, color='r', linestyle='--')

    # put a title at the top of the figure
    fig.suptitle('Bump Up/Down Aligned')
    plt.savefig('../static/images/bump/up_down_aligned_16.png')

    if SHOW_PLOT:
        plt.show()


report_orig()
report_v2()
report_min8_up()
report_all()
report_u128()
