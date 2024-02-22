#!/usr/bin/env python3
import numpy as np
from warnings import simplefilter
import matplotlib.pyplot as plt
from matplotlib.ticker import FuncFormatter
import seaborn as sns
sns.set()


SHOW_PLOT = False

# create a node type that can be used for a linked list
class Node:
    def __init__(self, data):
        self.data = data
        self.next = None

# create a linked list
class LinkedList:
    def __init__(self):
        self.head = None
        self.floyd_comparison_count = 0
        self.floyd_next_count = 0
        self.brent_comparison_count = 0
        self.brent_next_count = 0

    # add a new node to the linked list
    def append(self, data):
        new_node = Node(data)
        if self.head is None:
            self.head = new_node
            return
        last_node = self.head
        while last_node.next:
            last_node = last_node.next
        last_node.next = new_node

    # add a cycle from the last node to the first node
    def add_cycle(self):
        last_node = self.head
        while last_node.next:
            last_node = last_node.next
        last_node.next = self.head

    # print the linked list
    def print_list(self):
        cur_node = self.head
        while cur_node:
            print(cur_node.data, end=' ')
            cur_node = cur_node.next
        print()

    # detect a cycle in the linked list with floys algorithm
    def detect_cycle_floyd(self):
        slow = self.head
        fast = self.head
        while fast and fast.next:
            slow = slow.next
            fast = fast.next.next
            self.floyd_next_count += 3
            self.floyd_comparison_count += 1
            if slow == fast:
                return True
        return False

    # detect a cycle in the linked list with brent's algorithm
    def detect_cycle_brent(self):
        power = lam = 1
        tortoise = self.head
        hare = self.head
        while True and hare.next:
            if power == lam:
                tortoise = hare
                power *= 2
                lam = 0
            hare = hare.next
            lam += 1
            self.brent_comparison_count += 1
            self.brent_next_count += 1
            if tortoise == hare:
                return True
        return False

# for the range of n, create a linked list with a cycle and detect the cycle with floyd's and brent's algorithm
# then report the number of comparisons and next operations for each algorithm
def compare_algorithms(n):
    floyd_comparison = []
    floyd_next = []
    brent_comparison = []
    brent_next = []
    for i in range(2, n):
        ll = LinkedList()
        for j in range(1, i):
            ll.append(j)
        ll.add_cycle()
        ll.detect_cycle_floyd()
        floyd_comparison.append(ll.floyd_comparison_count)
        floyd_next.append(ll.floyd_next_count)
        ll.detect_cycle_brent()
        brent_comparison.append(ll.brent_comparison_count)
        brent_next.append(ll.brent_next_count)
    return floyd_comparison, floyd_next, brent_comparison, brent_next

   # create a linked list of 10 nodes with a cycle
def main():
    print('Detecting a cycle in a linked list')
    ll = LinkedList()
    for i in range(1, 11):
        ll.append(i)
    ll.print_list()
    ll.add_cycle()
    print(ll.detect_cycle_floyd())
    print(ll.detect_cycle_brent())
    print('Floyd comparison count:', ll.floyd_comparison_count)
    print('Brent comparison count:', ll.brent_comparison_count)
    print('Floyd next count:', ll.floyd_next_count)
    print('Brent next count:', ll.brent_next_count)

    # compare the two algorithms for a range of n
    n = 300
    floyd_comparison, floyd_next, brent_comparison, brent_next = compare_algorithms(n)

    # plot the number of comparisons for each algorithm
    x = np.arange(1, n-1)
    fig, ax = plt.subplots()
    ax.plot(x, floyd_comparison, label='Floyd')
    ax.plot(x, brent_comparison, label='Brent')
    ax.set_xlabel('cycle size')
    ax.set_ylabel('Count')
    ax.set_title('Total Comparisons')
    ax.legend()
    # add a vertical line at every power of two
    # for i in range(1, 9):
    #     ax.axvline(x=2**i, color='grey', linestyle='--')
    plt.savefig('../static/images/cycles/cycles_comparisons.png')
    if SHOW_PLOT:
        plt.show()

    # plot the number of next operations for each algorithm
    fig, ax = plt.subplots()
    ax.plot(x, floyd_next, label='Floyd')
    ax.plot(x, brent_next, label='Brent')
    ax.set_xlabel('cycle size')
    ax.set_ylabel('Count')
    ax.set_title('Total Next() calls')
    ax.legend()
    # for i in range(1, 9):
    #     ax.axvline(x=2**i, color='grey', linestyle='--')
    plt.savefig('../static/images/cycles/cycles_nexts.png')
    if SHOW_PLOT:
        plt.show()


if __name__ == '__main__':
    main()
