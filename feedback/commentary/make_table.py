import os, sys, subprocess

if len(sys.argv) < 2:
    sys.exit('ERROR: Please input TIME.\nexecute > python make_table.py {TIME}')

t = int(sys.argv[1])
I = 14
K = 3
texpath = f'./table{t}.tex'

cells = []
for i in range(I):
    cells.append([])
    for k in range(K):
        cells[i].append([])
        if os.path.isfile(f'./{t}/{i+1:02}_{k+1}.tex'):
            with open(f'./{t}/{i+1:02}_{k+1}.tex', 'r', encoding='utf-8') as f:
                cells[i][k].append(f'\\cellcolor{{c{i+1:02}{k+1}}}\\scriptsize\n')
                for row in f:
                    cells[i][k].append(row)
            cells[i][k] = f'{{{"".join(cells[i][k])}}}\n'
        else:
            cells[i][k] = '\\cellcolor{mygray}\n'

print(cells)

with open(texpath, 'w', encoding='utf-8') as f:
    f.write('\\noindent')
    f.write('\\begin{tabularx}{\\linewidth}{|c|X|X|X|}\n')
    f.write('\\hline\n')
    f.write('& \\multicolumn{1}{c|}{\\textbf{1. 代数的手続きの理解}} & \\multicolumn{1}{c|}{\\textbf{2. 関数的思考の理解}} & \\multicolumn{1}{c|}{\\textbf{3. 直交座標上での表現の理解}}\\\\\\hline\n')
    for i in range(I):
        f.write(f'\\scriptsize({i+1}) & {" & ".join(cells[i])}\\\\\\hline\n')
    f.write('\\end{tabularx}')