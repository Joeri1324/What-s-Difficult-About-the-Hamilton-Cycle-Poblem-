import matplotlib.pyplot as plt
import json
import sys
import numpy as np

algorithm, size = sys.argv[1], sys.argv[2]
file = './results/iterations/result-{0}-{1}.json'.format(algorithm, size)

with open(file) as f:
  fig = plt.figure()
  ax = plt.gca()
  ax.set_yscale('log')
  data = json.load(f)

  xs_true = [line['degree'] for line in data if line['hamiltonian'] == True]
  ys_true = [line['iterations'] for line in data if line['hamiltonian'] == True]

  xs_false = [line['degree'] for line in data if line['hamiltonian'] == False]
  ys_false = [line['iterations'] for line in data if line['hamiltonian'] == False]

  xs_un = [line['degree'] for line in data if line['hamiltonian'] == None]
  ys_un = [line['iterations'] for line in data if line['hamiltonian'] == None]

  # print(xs_false, ys_false)
  true = ax.scatter(xs_true, ys_true, label='Hamiltonian', color='#D81B60', alpha=0.3, s=10)
  false = ax.scatter(xs_false, ys_false, label='Non-Hamiltonian', color='#2979FF', alpha=0.3, s=10)
  un = ax.scatter(xs_un, ys_un, label='Unknown', color='#424242', alpha=0.5, s=7)
  ax.set_xlabel('Average Vertex Degree')
  ax.set_ylabel('Computational Cost (Number of Iterations)')
  ax.spines['right'].set_visible(False)
  ax.spines['top'].set_visible(False)

  ax.set_xticks(np.arange(0, int(size)))
  plt.legend(handles=[true, false, un])
  # plt.title(algorithm + " " + size)
  plt.show()
