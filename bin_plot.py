import matplotlib.pyplot as plt
import json
import sys
import numpy as np

measure = 'iterations'
folder = 'time' if measure is 'nanoseconds' else 'iterations'

def get_difficilty(filename):
  with open(filename) as f:
    return [line[measure] for line in json.load(f)]


def plot(title, ax, file):
  ax.set_yscale('log')
  ax.set_xscale('log')
  ax.set_ylabel('Volume of Instances')
  ax.set_xlabel('Number of {0}'.format(measure))
  ax.set_title(title)
  ax.set_ylim(bottom=1, top=10000)
  bins = [2 ** x for x in range(10, 30)]
  
  diff = get_difficilty(file)
  print(title, measure, 'mean', np.mean(diff), 'std', np.std(diff))
  ax.hist(diff, bins)


f, ((ax1, ax2), (ax3, ax4), (ax5, ax6)) = plt.subplots(3, 2, sharex='all', sharey='all')

plot("Vanhorn", ax1, 'results/{0}/result-vanhorn-32.json'.format(folder))
plot("Rubin", ax2, 'results/{0}/result-rubin-32.json'.format(folder))
plot("Vandegriend", ax3, 'results/{0}/result-vandegriend-32.json'.format(folder))
plot("Martello", ax4, 'results/{0}/result-martello-32.json'.format(folder))
plot("Naked Depth-First", ax5, 'results/{0}/result-nakeddepthfirst-32.json'.format(folder))
plot("Cheeseman", ax6, 'results/{0}/result-cheeseman-32.json'.format(folder))

plt.show()

