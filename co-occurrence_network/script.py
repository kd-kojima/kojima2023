# -*- coding: utf-8 -*-

#
# Kojima, K. (2023). Application of Cognitive Diagnosis Models (Senior thesis).
#   Script for co-occurrence network
#

# ----------------------
# Import modules
# 
import sys, math, itertools
import pandas as pd
import numpy as np
import matplotlib
from matplotlib import pyplot as plt
import networkx as nx

import MeCab
import neologdn, unicodedata

# ----------------------
# Set variables
# 
filename = 'integrated' # which text data to use

PLOT_FOR_THESIS = False
FIG_DRAW_REP_FOR_THESIS = 1

MIN_WORD_RATIO = {  # (word_freq / num_of_sentence)
    'course_guidelines': 0.02, # 0.02
    'integrated': 0.02, # 0.02
    'course_01': 0.022, # 0.022
    'course_02': 0.022, # 0.022
    'course_03': 0.028 # 0.028
} 
MIN_JACCARD = {
    'course_guidelines': 0.24, # 0.24
    'integrated': 0.24, # 0.24
    'course_01': 0.24, # 0.24
    'course_02': 0.30, # 0.30
    'course_03': 0.35 # 0.35
}

LAYOUT_K_FACTOR = {
    'course_guidelines': 2, # 2
    'integrated': 2, # 2
    'course_01': 2, # 2
    'course_02': 2, # 2
    'course_03': 1.5 # 1.5
}
LAYOUT_ITERATION = {
    'course_guidelines': 3000, # 3000
    'integrated': 3000, # 3000
    'course_01': 3000, # 3000
    'course_02': 3000, # 3000
    'course_03': 3000 # 3000
}
NODE_SIZE_FACTOR = {
    'course_guidelines': 30000,
    'integrated': 30000,
    'course_01': 30000,
    'course_02': 30000,
    'course_03': 30000
}
EDGE_WIDTH_FACTOR = {
    'course_guidelines': 8,
    'integrated': 8,
    'course_01': 8,
    'course_02': 8,
    'course_03': 8
}

min_word_ratio = MIN_WORD_RATIO[filename]
min_jaccard = MIN_JACCARD[filename]
layout_k_factor = LAYOUT_K_FACTOR[filename]
layout_iteration = LAYOUT_ITERATION[filename]
node_size_factor = NODE_SIZE_FACTOR[filename]
edge_width_factor = EDGE_WIDTH_FACTOR[filename]

IPA_DIC = '/opt/homebrew/lib/mecab/dic/ipadic'
USER_DIC = '/opt/homebrew/lib/mecab/dic/user/user.dic'

stopwords_path = './stopwords.txt'

raw_dir = './rawdata'
text_dir = './textdata'
noun_dir = './noundata'
csv_dir = './csv'
result_dir = './result'

thesis_dir = './thesis_fig'

# ----------------------
# Read stopwords
# 
stopwords = []
with open(stopwords_path, 'r', encoding='utf-8') as f:
    stopwords = f.read().split('\n')

# ----------------------
# Read rawdata
# 
rawdata = ''
with open(f'{raw_dir}/{filename}.txt', 'r', encoding='utf-8') as f:
    rawdata = f.readlines()

# ----------------------
# Split sentence
# 
rawdata = ''.join(rawdata).replace('\n', '').replace('。','。\n')
rawdata = neologdn.normalize(rawdata)
rawdata = unicodedata.normalize('NFKC', rawdata)

with open(f'{text_dir}/{filename}.txt', 'w', encoding='utf-8') as f:
    f.write(rawdata)

# ----------------------
# Read textdata
# 
textdata = []
with open(f'{text_dir}/{filename}.txt', 'r', encoding='utf-8') as f:
    textdata = f.readlines()

textdata = [line.replace('\n', '') for line in textdata]

# ----------------------
# Get nouns
# 
nouns = []
mecab = MeCab.Tagger(f'-d \"{IPA_DIC}\" -u \"{USER_DIC}\" -Odump')
for line in textdata:
    morph = [x.split(' ') for x in mecab.parse(line).split('\n')][:-1]
    nouns.append([x[1] for x in morph if '名詞' in x[2] and x[1] not in stopwords])

with open(f'{noun_dir}/{filename}.txt', 'w', encoding='utf-8') as f:
    for line in nouns:
        f.write(','.join(line))
        f.write('\n')

nouns = [set(line) for line in nouns]
num_of_sentences = len(nouns)

# ----------------------
# Count nouns
# 
freqs = {}
for word in list(itertools.chain.from_iterable(nouns)):
    freqs[word] = freqs.get(word, 0) + 1

freqs_df = pd.DataFrame.from_dict(freqs, orient='index', columns=['freq'])

plt.rcParams['font.family'] = 'Hiragino Sans'
plt.bar(freqs_df.sort_values('freq', ascending=False)[0:50].index.values, freqs_df.sort_values('freq', ascending=False)['freq'][0:50])
plt.show()

# ----------------------
# Make combinations
# 
combinations = [list(itertools.combinations(set(line), 2)) for line in nouns]
combinations = [[tuple(sorted(combi)) for combi in line] for line in combinations]

# ----------------------
# Count combinations
# 
combi_freqs = {}
for combi in list(itertools.chain.from_iterable(combinations)):
    combi_freqs[combi] = combi_freqs.get(combi, 0) + 1

combi_df = pd.DataFrame([[key[0], key[1], value] for key, value in combi_freqs.items()], columns=['w1', 'w2', 'freq'])

# ----------------------
# Calculate Jaccard coef
# 
w1_freq = []
w1_ratio = []
w2_freq = []
w2_ratio = []
jaccards = []
for _, row in combi_df.iterrows():
    jaccard = 1
    union = freqs_df.loc[row.w1].freq + freqs_df.loc[row.w2].freq - row.freq
    if union != 0:
        jaccard = row.freq / union
    w1_freq.append(freqs_df.loc[row.w1].freq)
    w1_ratio.append(freqs_df.loc[row.w1].freq / num_of_sentences)
    w2_freq.append(freqs_df.loc[row.w2].freq)
    w2_ratio.append(freqs_df.loc[row.w2].freq / num_of_sentences)
    jaccards.append(jaccard)

# ----------------------
# Make dataframe
# 
combi_df['w1_freq'] = w1_freq
combi_df['w1_ratio'] = w1_ratio
combi_df['w2_freq'] = w2_freq
combi_df['w2_ratio'] = w2_ratio
combi_df['jaccard'] = jaccards
combi_df.sort_values('jaccard', ascending=False).to_csv(f'{csv_dir}/{filename}_df.csv')

# ----------------------
# Set graph
# 
G = nx.Graph()
G.add_nodes_from(freqs_df.index.values)
for _, row in combi_df.iterrows():
    if row.w1_ratio >= min_word_ratio and row.w2_ratio >= min_word_ratio and row.jaccard >= min_jaccard:
        G.add_edge(row.w1, row.w2, weight=row.jaccard)

G.remove_nodes_from(list(nx.isolates(G)))

if filename == 'integrated':
    # because the subgraph composed from following two nodes is isolated from other subgraphs (even if thin edges are added).
    G.remove_node('知識')
    G.remove_node('技能')

# get subgraphs
connecteds = []
connected_indices = []
colors = []
for i, c in enumerate(nx.connected_components(G)):
    connecteds.append(c)
    connected_indices.append(i)
    colors.append(1/50 * i)

# set colors
node_colors = []
for node in G.nodes():
    for i, c in enumerate(connecteds):
        if node in c:
            node_colors.append(colors[i])
            break

# add thin edges
for _, row in combi_df.iterrows():
    if row.w1 in list(G.nodes()) and row.w2 in list(G.nodes()):
        G.add_edge(row.w1, row.w2, weight=row.jaccard)

# get edge list
thick_edges = []
thin_edges = []
for i, edge in enumerate(G.edges(data='weight')):
    w1, w2, weight = edge
    if weight >= min_jaccard:
        thick_edges.append(i)
    else:
        thin_edges.append(i)

# ----------------------
# Plot graph
# 
if not PLOT_FOR_THESIS:
    plt.figure(figsize=(8, 8))

    k = layout_k_factor / math.sqrt(len(G.nodes()))
    layout = nx.spring_layout(G, k=k, iterations=layout_iteration)

    pr = nx.pagerank(G)
    pr_values = np.array([pr[node] for node in G.nodes()])
    nx.draw_networkx_nodes(G, layout, node_color=node_colors, cmap=plt.cm.get_cmap('Set3'), alpha=0.7, node_size=pr_values * node_size_factor)

    edge_width = [weight * edge_width_factor for _, _, weight in G.edges(data='weight')]
    nx.draw_networkx_edges(G, layout, edgelist=tuple([list(G.edges())[i] for i in thick_edges]), alpha=0.4, edge_color='#999999', width=[edge_width[i] for i in thick_edges])
    nx.draw_networkx_edges(G, layout, edgelist=tuple([list(G.edges())[i] for i in thin_edges]), style='dashed', alpha=0.4, edge_color='#bbbbbb', width=[edge_width[i] for i in thin_edges])

    nx.draw_networkx_labels(G, layout, font_family='Hiragino Sans', font_size=10, font_weight='bold')

    plt.axis('off')
    # plt.savefig(f'{result_dir}/{filename}.png', dpi=300)
    plt.show()

    sys.exit()

# ----------------------
# Plot for thesis
# 
matplotlib.use('pgf')
plt.rcParams['text.usetex'] = True 
plt.rcParams['pgf.texsystem'] = 'lualatex'
plt.rcParams['pgf.preamble'] = r'\usepackage{unicode-math}\setmainfont{IPAexGothic}\setmathfont{Fira Math}'
plt.rcParams['pgf.rcfonts'] = False

print('Groups:')
for connected in connecteds:
    print(connected)

color_thesis = {
    'A7': '#c3d825',
    'A8': '#59b9c6',
    'A9': '#5383c3',
}
nodes_thesis = {
    'A7': [],
    'A8': [],
    'A9': [],
    'others': []
}

# Classify groups
for connected in connecteds:
    if 'グラフ' in connected and '式' in connected:
        nodes_thesis['A9'].extend([list(G.nodes()).index(node) for node in connected])
    elif '座標' in connected and '平面' in connected:
        nodes_thesis['A9'].extend([list(G.nodes()).index(node) for node in connected])
    elif '変化の割合' in connected and '一定' in connected:
        nodes_thesis['A9'].extend([list(G.nodes()).index(node) for node in connected])
    elif 'x' in connected and 'y' in connected:
        nodes_thesis['A7'].extend([list(G.nodes()).index(node) for node in connected])
    elif '一方' in connected and '他方' in connected:
        nodes_thesis['A7'].extend([list(G.nodes()).index(node) for node in connected])
    elif '日常' in connected:
        nodes_thesis['A8'].extend([list(G.nodes()).index(node) for node in connected])
    elif '実験' in connected and '予測' in connected:
        nodes_thesis['A8'].extend([list(G.nodes()).index(node) for node in connected])
    elif '実験' in connected and '観察' in connected:
        nodes_thesis['A8'].extend([list(G.nodes()).index(node) for node in connected])
    elif '具体' in connected and '事象' in connected:
        nodes_thesis['A8'].extend([list(G.nodes()).index(node) for node in connected])
    elif '円' in connected and '方程式' in connected:
        nodes_thesis['A9'].extend([list(G.nodes()).index(node) for node in connected])
    elif '不等式' in connected and '領域' in connected:
        nodes_thesis['A9'].extend([list(G.nodes()).index(node) for node in connected])
    elif '定積分' in connected and '面積' in connected:
        nodes_thesis['A9'].extend([list(G.nodes()).index(node) for node in connected])
    elif '関数関係' in connected and '意味' in connected:
        nodes_thesis['A7'].extend([list(G.nodes()).index(node) for node in connected])
    elif '概念' in connected and '実感' in connected:
        nodes_thesis['A7'].extend([list(G.nodes()).index(node) for node in connected])
    elif '交点' in connected and '意味' in connected:
        nodes_thesis['A9'].extend([list(G.nodes()).index(node) for node in connected])
    else:
        nodes_thesis['others'].extend([list(G.nodes()).index(node) for node in connected])

for cnt in range(FIG_DRAW_REP_FOR_THESIS):
    k = layout_k_factor / math.sqrt(len(G.nodes()))
    layout = nx.spring_layout(G, k=k, iterations=layout_iteration)
    layout_thesis = {}
    for i, pos in enumerate(layout.values()):
        layout_thesis[i] = pos

    pr = nx.pagerank(G)
    pr_values = np.array([pr[node] for node in G.nodes()])

    edge_width = [weight * edge_width_factor for _, _, weight in G.edges(data='weight')]

    # Plot classified graph
    plt.figure(figsize=(8, 8))

    nx.draw_networkx_nodes(G, layout_thesis, nodelist=nodes_thesis['others'], node_color='#ffffff', alpha=0.7, node_size=pr_values[nodes_thesis['others']] * node_size_factor, edgecolors='#aaaaaa')
    nx.draw_networkx_nodes(G, layout_thesis, nodelist=nodes_thesis['A9'], node_color=color_thesis['A9'], alpha=0.75, node_size=pr_values[nodes_thesis['A9']] * node_size_factor)
    nx.draw_networkx_nodes(G, layout_thesis, nodelist=nodes_thesis['A8'], node_color=color_thesis['A8'], alpha=0.45, node_size=pr_values[nodes_thesis['A8']] * node_size_factor * 0.7, linewidths=1.0, edgecolors=color_thesis['A8'])
    nx.draw_networkx_nodes(G, layout_thesis, nodelist=nodes_thesis['A8'], node_color=color_thesis['A8'], alpha=0.3, node_size=pr_values[nodes_thesis['A8']] * node_size_factor)
    nx.draw_networkx_nodes(G, layout_thesis, nodelist=nodes_thesis['A7'], node_color=color_thesis['A7'], alpha=0.6, node_size=pr_values[nodes_thesis['A7']] * node_size_factor * 0.3, linewidths=3.0, edgecolors=color_thesis['A7'])
    nx.draw_networkx_nodes(G, layout_thesis, nodelist=nodes_thesis['A7'], node_color=color_thesis['A7'], alpha=0.4, node_size=pr_values[nodes_thesis['A7']] * node_size_factor)

    nx.draw_networkx_edges(G, layout, edgelist=tuple([list(G.edges())[i] for i in thick_edges]), alpha=0.4, edge_color='#999999', width=[edge_width[i] for i in thick_edges])
    nx.draw_networkx_edges(G, layout, edgelist=tuple([list(G.edges())[i] for i in thin_edges]), style='dashed', alpha=0.4, edge_color='#bbbbbb', width=[edge_width[i] for i in thin_edges])

    nx.draw_networkx_labels(G, layout, font_family='Hiragino Sans', font_size=10)

    plt.scatter([], [], c=color_thesis['A7'], label='$\mathrm{A7_c}$', alpha=0.7)
    plt.scatter([], [], c=color_thesis['A8'], label='$\mathrm{A8_c}$', alpha=0.7)
    plt.scatter([], [], c=color_thesis['A9'], label='$\mathrm{A9_c}$', alpha=0.7)

    plt.axis('off')
    plt.legend()
    plt.savefig(f'{thesis_dir}/{filename}_{cnt:03}.pdf', dpi=300)
    plt.show()

    # Plot unclassified graph
    plt.figure(figsize=(8, 8))

    nx.draw_networkx_nodes(G, layout, node_color=node_colors, cmap=plt.cm.get_cmap('YlOrRd_r'), alpha=0.8, node_size=pr_values * node_size_factor)

    nx.draw_networkx_edges(G, layout, edgelist=tuple([list(G.edges())[i] for i in thick_edges]), alpha=0.4, edge_color='#999999', width=[edge_width[i] for i in thick_edges])
    nx.draw_networkx_edges(G, layout, edgelist=tuple([list(G.edges())[i] for i in thin_edges]), style='dashed', alpha=0.4, edge_color='#bbbbbb', width=[edge_width[i] for i in thin_edges])

    nx.draw_networkx_labels(G, layout, font_family='Hiragino Sans', font_size=10)

    for i, c in enumerate(colors):
        plt.scatter([], [], c=plt.cm.get_cmap('YlOrRd_r')((i+1)/len(colors)), label=f'{i+1}', alpha=0.8)

    plt.axis('off')
    plt.legend()
    plt.savefig(f'{thesis_dir}/{filename}_unclassified_{cnt:03}.pdf', dpi=300)
    plt.show()