import matplotlib.pyplot as plt
import json
import sys
import numpy as np

algorithm, size = sys.argv[1], sys.argv[2]
file = './vanhornresults/{0}{1}.csv'.format(algorithm, size)

with open(file) as f:
  fig = plt.figure()
  ax = plt.gca()
  ax.set_yscale('log')
  data = [line.split(",") for line in f]
  xs_true   = [
    float(line[1])
    for line in data
    if line[2] == 'true'
  ]
  ys_true = [float(line[3].replace('\n', '')) for line in data if line[2] == 'true']
  xs_false  = [
    float(line[1])
    for line in data
    if line[2] == 'false'
  ]
  ys_false = [float(line[3].replace('\n', '')) + 1 for line in data if line[2] == 'false']

  xs_un  = [
    float(line[1])
    for line in data
    if line[2] == 'unknown'
  ]
  ys_un = [float(line[3].replace('\n', '')) + 1 for line in data if line[2] == 'unknown']

  # print(xs_false, ys_false)
  true = ax.scatter(xs_true, ys_true, label='Hamiltonian', color='#D81B60', alpha=0.3, s=7)
  false = ax.scatter(xs_false, ys_false, label='Non-Hamiltonian', color='#2979FF', alpha=0.3, s=7)
  un = ax.scatter(xs_un, ys_un, label='Unknown', color='#424242', alpha=0.5, s=7)
  ax.set_xlabel('Average Vertex Degree')
  ax.set_ylabel('Computational Cost (Number of Iterations)')
  ax.spines['right'].set_visible(False)
  ax.spines['top'].set_visible(False)

  ax.set_xticks(np.arange(0, int(size)))
  plt.legend(handles=[true, false, un])
  # plt.title(algorithm + " " + size)
  plt.show()
