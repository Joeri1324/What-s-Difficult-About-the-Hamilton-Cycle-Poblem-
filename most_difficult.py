import sys
import matplotlib.pyplot as plt
import json
import networkx as nx


algorithm, size = sys.argv[1], sys.argv[2]
file = './results/iterations/result-{0}-{1}.json'.format(algorithm, size)

with open(file) as f:
  fig = plt.figure()
  ax = plt.gca()
  ax.set_yscale('log')
  data = json.load(f)

difficulty = [line for line in data]
difficulty.sort(key=lambda x: x['iterations'])

id = difficulty[-1]['id']
print(difficulty[-1])
with open('src/main/resources/indexed-32-node-test-set/{0}.json'.format(id)) as f:
    plt.figure()
    G = nx.Graph()
    graph = json.load(f)
    G.add_nodes_from([int(k) for k in graph['connectivityMap'].keys()])
    for edge in graph['edges']:
        G.add_edge(int(edge['endpoints'][0]['id']), int(edge['endpoints'][1]['id']))
    # nx.draw(G)
    # nx.draw_random(G)
    # nx.draw_circular(G)
    nx.draw_circular(G,  node_size=500, font_size=8)
    nx.draw_networkx_labels(G, pos=nx.circular_layout(G))
    plt.savefig('after.png')
    # plt.tight_layout()
    plt.show()
