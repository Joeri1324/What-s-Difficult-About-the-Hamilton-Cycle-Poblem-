import json
import itertools
import matplotlib.pyplot as plt
from collections import defaultdict

def group(data):
    result = defaultdict(list)
    for d in data:
        result[d['degree']].append(
            d['nanoseconds']
        )
    return result

def to_mean(grouped):
    mean = lambda x: sum(x) / len(x)
    return [
        (a, mean(b)) for a, b in grouped.items()
    ]

def plot_avg_file(file_name, label):
    with open(file_name) as f:
        data = sorted(to_mean(group(json.load(f))), key=lambda x: x[0])
        x, y = zip(*data)
        return plt.plot(x, y, label=label, alpha=0.5)
    
fig = plt.figure()
ax = plt.gca()
ax.set_yscale('log')

a = plot_avg_file('results/time/result-vanhorn-32.json', 'vanhorn')
b = plot_avg_file('results/time/result-rubin-32.json', 'rubin')
c = plot_avg_file('results/time/result-martello-32.json', 'martello')
d = plot_avg_file('results/time/result-vandegriend-32.json', 'vandegriend')
e = plot_avg_file('results/time/result-nakeddepthfirst-32.json', 'nakeddepthfirst')
f = plot_avg_file('results/time/result-cheeseman-32.json', 'cheeseman')

plt.xlabel("Average Vertex Degree")
plt.ylabel("Computational Cost (Number of Nanoseconds)")
plt.legend(handles=a + b + c + d + e + f)
plt.show()

