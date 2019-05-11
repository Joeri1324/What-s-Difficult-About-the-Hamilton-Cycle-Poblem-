import matplotlib.pyplot as plt
import networkx as nx
G = nx.cycle_graph(80)
pos = nx.circular_layout(G)
# default
plt.figure(1)
nx.draw(G,pos)
# smaller nodes and fonts
plt.figure(2)
nx.draw(G,pos,node_size=60,font_size=8) 
# larger figure size
plt.figure(3,figsize=(12,12)) 
nx.draw(G,pos)
plt.show()