import matplotlib.pyplot as plt
import json
import sys
import numpy as np
from collections import defaultdict

measure = 'nanoseconds'
folder = 'time' if measure is 'nanoseconds' else measure

def grouped(data, key):
    result = defaultdict(list)
    for item in data:
        result[item[key]] += item['nanoseconds']


def plot(file, ax, title):
    with open(file) as f:
        ax.set_yscale('log')
        data = json.load(f)

        xs_true = [line['degree'] for line in data if line['hamiltonian'] == True]
        ys_true = [line[measure] for line in data if line['hamiltonian'] == True]

        xs_false = [line['degree'] for line in data if line['hamiltonian'] == False]
        ys_false = [line[measure] for line in data if line['hamiltonian'] == False]

        xs_un = [line['degree'] for line in data if line['hamiltonian'] == None]
        ys_un = [line[measure] for line in data if line['hamiltonian'] == None]

        # print(xs_false, ys_false)
        true = ax.scatter(xs_true, ys_true, label='Hamiltonian', color='#D81B60', alpha=0.3, s=10)
        false = ax.scatter(xs_false, ys_false, label='Non-Hamiltonian', color='#2979FF', alpha=0.3, s=10)
        un = ax.scatter(xs_un, ys_un, label='Unknown', color='#424242', alpha=0.5, s=7)
        ax.set_title(title)
        ax.set_xlabel('Average Vertex Degree')
        ax.set_ylabel('Computational Cost (Number of Nanoseconds)')
        ax.spines['right'].set_visible(False)
        ax.spines['top'].set_visible(False)

        ax.set_xticks(np.arange(0, int(32)))
        ax.legend(handles=[true, false, un])

        means = grouped(data, 'degree')
        # print(title, )



file1 = 'results/{0}/result-vanhorn-32.json'.format(folder)
file2 = 'results/{0}/result-rubin-32.json'.format(folder)
file3 = 'results/{0}/result-martello-32.json'.format(folder)
file4 = 'results/{0}/result-vandegriend-32.json'.format(folder)
file5 = 'results/{0}/result-nakeddepthfirst-32.json'.format(folder)
file6 = 'results/{0}/result-cheeseman-32.json'.format(folder)

f, ((ax1, ax2), (ax3, ax4), (ax5, ax6)) = plt.subplots(3, 2, sharex='col', sharey='row')

plot(file1, ax1, "Van Horn")
plot(file2, ax2, "Rubin")
plot(file3, ax3, "Martello")
plot(file4, ax4, "Vandegriend")
plot(file5, ax5, "Naked Depth-First")
plot(file6, ax6, "Cheeseman")
plt.show()

