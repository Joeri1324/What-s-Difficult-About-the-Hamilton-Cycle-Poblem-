import matplotlib.pyplot as plt
import json
import sys
import numpy as np

algorithm, size = sys.argv[1], sys.argv[2]
file = './results/{0}{1}.csv'.format(algorithm, size)

with open(file) as f:
  fig = plt.figure()
  ax = plt.gca()
  ax.set_yscale('log')
  data = [line.split(",") for line in f]
  difficulty = [
    float(line[3])
    for line in data
  ]
  
  bins = np.histogram(difficulty, 10)[1]
  print("bins", bins, difficulty)
  plt.hist(difficulty, 
    bins
  )
  plt.show()
  print()
