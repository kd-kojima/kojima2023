#
# Kojima, K. (2023). Application of Cognitive Diagnosis Models (Senior thesis).
#   Script for making feedback sheets
#

import os, sys, subprocess

# check table{t}.tex is updated
# execute > python make_feedback.py TIME

if len(sys.argv) < 2:
    sys.exit('ERROR: Please input TIME.\nexecute > python make_feedback.py {TIME}')

# --------------------
# indices
#

# data
ID = 0
ANS = slice(1,15)
TIME = 15
YEAR = 16
SEX = 17

# mastery probability
MP = slice(1,4)

# --------------------
# paths
#
t = int(sys.argv[1])

dir = f'./t{t}_feedback'
temp_dir = './templates'
data_file = f't{t}.csv'
prob_file = []
for i in range(t):
    prob_file.append(f'prob_T{t}_{i+1}.csv')
if t > 1:
    full_file = f'full_T{t}_{t}.csv'
qmat_file = f'./qmat1.csv'
all_sheet_file = f'./all_sheet{t}'

commentary_dir = './commentary'
table_file = f'{commentary_dir}/table{t}.tex'

# --------------------
# read csv
#

# data
data = []
with open(data_file, 'r', encoding='utf-8') as f:
    f.readline() # skip header
    for row in f:
        data.append([int(x) for x in row.replace('\n', '').split(',')])

# mastery probability
prob = []
for i in range(len(prob_file)):
    prob.append([])
    with open(prob_file[i], 'r', encoding='utf-8') as f:
        f.readline() # skip header
        for row in f:
            prob[i].append([float(x) for x in row.replace('\n', '').split(',')])

if t == 1:
    full = data
else:
    full = []
    with open(full_file, 'r', encoding='utf-8') as f:
        f.readline() # skip header
        for row in f:
            full.append([int(x) if x != 'NA' else x for x in row.replace('\n', '').split(',')])

# --------------------
# get templates
#
title = []
with open(f'{temp_dir}/title_t{t}.tex', 'r', encoding='utf-8') as f:
    for row in f:
        title.append(row)

graph = []
with open(f'{temp_dir}/graph_t{t}.tex', 'r', encoding='utf-8') as f:
    for row in f:
        graph.append(row)

comment = []
with open(f'{temp_dir}/comment.tex', 'r', encoding='utf-8') as f:
    for row in f:
        comment.append(row)

general = []
with open(f'{temp_dir}/general.tex', 'r', encoding='utf-8') as f:
    for row in f:
        general.append(row)

table = []
with open(table_file, 'r', encoding='utf-8') as f:
    for row in f:
        table.append(row)

template = []
with open(f'{temp_dir}/template.tex', 'r', encoding='utf-8') as f:
    for row in f:
        template.append(row)

# --------------------
# get comments
#
comments = []
for i in range(3):
    comments.append([])
    with open(f'{temp_dir}/comment_A{i+1}.txt') as f:
        comments[i] = f.read().split('\n\n')

# --------------------
# make general information
# 

# rate of correct
sumcol = [sum(col) for col in zip(*data)][ANS]
rate = [round(n * 100 / len(data), 1) for n in sumcol]
mean = round(sum(sumcol) / len(data), 1)

# difficulty
def get_difficulty(rate):
    if rate >= 66.6:
        return '\\easy'
    elif 33.3 <= rate and rate < 66.6:
        return '\\medium'
    else:
        return '\\hard'
difficulty = [get_difficulty(n) for n in rate]

# q-matrix
qmat = []
cnt = 0
with open(qmat_file, 'r', encoding='utf-8') as f:
    f.readline() # skip header
    for row in f:
        qmat.append([])
        qs = row.split(',')[1:]
        for i in range(len(qs)):
            if int(qs[i]) == 1:
                qmat[cnt].append(str(i+1))
        cnt = cnt + 1

rate = [f'{{\\small {n}}}' for n in rate]
qmat = [', '.join(n) for n in qmat]
qmat = [f'{{\\small {n}}}' for n in qmat]

general_info = []
general_info.append(f'正答数：\\textbf{{\\Large :::}}問 /{len(qmat)}　（平均\\textbf{{{mean}}}問）\n')
general_info.append('\\textbf{\\small 正誤} & ::: \\\\\n')
general_info.append(f'\\textbf{{\\small 正答率}} {{\\small (\\%)}} & {" & ".join(rate)}\\\\\n')
general_info.append(f'\\textbf{{\\small 難易度}} & {" & ".join(difficulty)}\\\\\n')
general_info.append(f'\\textbf{{\\small 測定能力}} & {" & ".join(qmat)}\\\\\n')


# --------------------
# make sheet
#
all_sheet = []
all_sheet.extend(template[:-2])

for i in range(len(full)):
    if full[i][ANS][0] == 'NA':
        continue
    tex = []
    # mps = prob[i][MP]
    # answer = data[i][ANS]
    tex.append('\\newgeometry{left=2cm, right=2cm, top=1cm, bottom=1cm}\n')
    tex.append(f'\\rhead{{}}\n')

    for row in title:
        if '<here>' in row:
            tex.append(f'\\hspace*{{5em}}番号：{int(full[i][ID]):07}\\\\\n')
        else:
            tex.append(row)

    tex.append('\n')
    for row in graph:
        if '<here>' in row:
            if t == 1:
                for j in range(len(prob[0][i][MP])):
                    mp = prob[0][i][MP][j]
                    tex.append(f'\\fill[gray!{int(mp*100 + 20) if mp*100 + 20 < 100 else 100}!red] (0,{j*(-1)+3.3}) rectangle ({mp*10 if mp*10 > 0.1 else 0.1},{j*(-1)+2.7}); \\draw ({mp*10 if mp*10 > 0.1 else 0.1},{j*(-1)+3}) node[right]{{\\scriptsize\\textbf{{{round(mp*100)}}}\\%}};\n')
            elif t == 2:
                for j in range(len(prob[0][i][MP])):
                    mp = prob[0][i][MP][j]
                    tex.append(f'\\fill[lightgray] (0,{j*(-1.1)+2.6}) rectangle ({mp*10 if mp*10 > 0.1 else 0.1},{j*(-1.1)+2.8});')
                for j in range(len(prob[1][i][MP])):
                    mp = prob[1][i][MP][j]
                    tex.append(f'\\fill[gray!{int(mp*100 + 20) if mp*100 + 20 < 100 else 100}!red] (0,{j*(-1.1)+3.4}) rectangle ({mp*10 if mp*10 > 0.1 else 0.1},{j*(-1.1)+2.8}); \\draw ({mp*10 if mp*10 > 0.1 else 0.1},{j*(-1.1)+3.1}) node[right]{{\\scriptsize\\textbf{{{round(mp*100)}}}\\%}};\n')
            elif t == 3:
                for j in range(len(prob[0][i][MP])):
                    mp = prob[0][i][MP][j]
                    tex.append(f'\\fill[lightgray] (0,{j*(-1.1)+2.5}) rectangle ({mp*10 if mp*10 > 0.1 else 0.1},{j*(-1.1)+2.7});')
                for j in range(len(prob[1][i][MP])):
                    mp = prob[1][i][MP][j]
                    tex.append(f'\\fill[lightgray] (0,{j*(-1.1)+2.7}) rectangle ({mp*10 if mp*10 > 0.1 else 0.1},{j*(-1.1)+2.9});')
                for j in range(len(prob[2][i][MP])):
                    mp = prob[2][i][MP][j]
                    tex.append(f'\\fill[gray!{int(mp*100 + 20) if mp*100 + 20 < 100 else 100}!red] (0,{j*(-1.1)+3.4}) rectangle ({mp*10 if mp*10 > 0.1 else 0.1},{j*(-1.1)+2.9}); \\draw ({mp*10 if mp*10 > 0.1 else 0.1},{j*(-1.1)+3.15}) node[right]{{\\scriptsize\\textbf{{{round(mp*100)}}}\\%}};\n')
        else:
            tex.append(row)

    tex.append('\n')
    cnt = 0
    for row in comment:
        if '<here>' in row:
            tex.append(f'{comments[cnt][int(prob[t-1][i][MP][cnt] * 4) if int(prob[t-1][i][MP][cnt] * 4) < 4 else 3]}\n')
            cnt = cnt + 1
        else:
            tex.append(row)

    tex.append('\n')
    cnt = 0
    for row in general:
        if '<here>' in row:
            if cnt == 0:
                if full[i][ANS][0] == 'NA':
                    tex.append(general_info[cnt].replace(':::', ' - '))
                else:
                    tex.append(general_info[cnt].replace(':::', str(sum(full[i][ANS]))))
            elif cnt == 1:
                if full[i][ANS][0] == 'NA':
                    tex.append(general_info[cnt].replace(':::', ' & '.join(['-' for _ in full[i][ANS]])))
                else:
                    tex.append(general_info[cnt].replace(':::', ' & '.join([str(n).replace('1', '\\corr').replace('0', '\\fail') for n in full[i][ANS]])))
            else:
                tex.append(general_info[cnt])
            cnt = cnt + 1
        else:
            tex.append(row)

    # --------------------------------
    # commentary
    #
    tex.append('\n\\clearpage\n\n')
    tex.append('\\newgeometry{left=1.5cm, right=1.5cm, top=1.5cm, bottom=0.5cm}\n')
    tex.append(f'\\rhead{{{int(full[i][ID]):07}}}\n')
    Q = []
    with open(qmat_file, 'r', encoding='utf-8') as f:
        f.readline() # skip header
        for row in f:
            Q.append(row.split(',')[1:])
    for item in range(len(Q)):
        for k in range(len(Q[0])):
            if int(Q[item][k]) == 1:
                if int(prob[t-1][i][MP][k] * 4) < 2:
                    tex.append(f'\\definecolor{{c{item+1:02}{k+1}}}{{rgb}}{{1.0,0.85,0.85}}\n')
                else:
                    tex.append(f'\\definecolor{{c{item+1:02}{k+1}}}{{rgb}}{{1.0,1.0,1.0}}\n')

    for row in table:
        tex.append(row)

    # ---------------------
    # write
    #
    with open(f'{dir}/{int(full[i][ID]):04}.tex', 'w', encoding='utf-8') as f:
        for row in tex:
            f.write(f'{row}')

    all_sheet.append(f'\\input{{{dir}/{int(full[i][ID]):04}.tex}}\\newpage\n')

# --------------------
# combine all
#
all_sheet.extend(template[-2:])
with open(f'{all_sheet_file}.tex', 'w', encoding='utf-8') as f:
    for row in all_sheet:
        f.write(row)

subprocess.run(['uplatex', all_sheet_file])
subprocess.run(['dvipdfmx', all_sheet_file])