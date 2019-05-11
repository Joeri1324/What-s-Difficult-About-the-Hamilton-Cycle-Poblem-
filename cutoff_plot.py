import matplotlib.pyplot as plt
import json


def plot(file, ax, title):
    with open(file) as f:
        data = json.load(f)

        xs = [line['degree'] for line in data if line['hamiltonian'] == None]
        ax.set_title(title)
        ax.hist(xs, color='black', alpha=0.7)


file1 = 'results/result-vanhorn-32.json'
file2 = 'results/result-rubin-32.json'
file3 = 'results/result-martello-32.json'
file4 = 'results/result-vandegriend-32.json'
file5 = 'results/result-nakeddepthfirst-32.json'
file6 = 'results/result-cheeseman-32.json'

f, ((ax1, ax2), (ax3, ax4), (ax5, ax6)) = plt.subplots(3, 2, sharex='col', sharey='row')

plot(file1, ax1, "Van Horn")
plot(file2, ax2, "Rubin")
plot(file3, ax3, "Martello")
plot(file4, ax4, "Vandegriend")
plot(file5, ax5, "Naked Depth-First")
plot(file6, ax6, "Cheeseman")
plt.show()