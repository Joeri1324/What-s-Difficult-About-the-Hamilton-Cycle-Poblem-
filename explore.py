import json

type = "time"
data = [
    list(json.load(open("results/{0}/result-rubin-32.json".format(type)))),
    list(json.load(open("results/{0}/result-martello-32.json".format(type)))),
    list(json.load(open("results/{0}/result-vandegriend-32.json".format(type)))),
    list(json.load(open("results/{0}/result-vanhorn-32.json".format(type)))),
    list(json.load(open("results/{0}/result-cheeseman-32.json".format(type)))),
    list(json.load(open("results/{0}/result-nakeddepthfirst-32.json".format(type)))),
    list(json.load(open("results/{0}/result-sleegers-32.json".format(type))))
]

# def exclusive_min(algos):
#     minimum = min(algos, key= lambda x: x['iterations'])
#     if sum(1 for l in algos if l['iterations'] == minimum['iterations']) > 1 or minimum['iterations'] == 31:
#         return None
#     else:
#         if minimum['algorithm'] == 'vanhorn':
#             print('!!!!!WJAY!!!!!', [(a['algorithm'], a['hamiltonian'], a['iterations']) for a in algos])
#             print()
#         return minimum


# r = [(exclusive_min(algos)['algorithm'], exclusive_min(algos)['iterations'], exclusive_min(algos)['id']) 
#      for algos in zip(*data) if exclusive_min(algos)]
# print(r)


# van = list(json.load(open("results/iterations/result-vandegriend-32.json")))
# sle = list(json.load(open("results/iterations/result-sleegers-32.json")))

# # print(sum(1 for l in van if l['iterations'] > 100))
# # print(sum(1 for l in sle if l['iterations'] > 100))

print(len(data[0]))

for algo in data:
    name = algo[0]['algorithm']
    print(
        name, len([x for x in algo if x['hamiltonian'] is None])
    )
