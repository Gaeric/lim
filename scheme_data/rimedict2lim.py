#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Filename: rimedict2lim.py
# Datetime: Thu Dec 16 23:22:57 2021
# Software: Emacs
# Author:   Gaeric


punc = '''
{ 『
| ÷
} 』
` ・
! ！
$ ￥
% ％
& ※
( （
) ）
* ×
+ ＋
, ，
. 。
/ 、
: ：
; ；
< 《
= ＝
> 》
? ？
[ 【
] 】
^ ……
_ ──
" “ ”
' ‘ ’
'''

table = {}
with open('./sbxlm/sbfd.dict.yaml') as fp:
    content = fp.readlines()
    start = False
    first_char = set()
    total_char = set()
    for line in content:
        if start:
            line_content = line.split()
            chara = line_content[0]
            encode = line_content[1]
            if table.get(encode):
                table[encode] += f' {chara}'
            else:
                table[encode] = f'{chara}'
                first_char.add(encode[0])
            total_char.update(list(encode))
        elif line.startswith('...'):
            start = True

first_char = [x for x in first_char]
first_char.sort()
total_char = [x for x in total_char]
total_char.sort()
with open('reuslt.txt', 'w', encoding='utf-8') as fp:
    fp.write(";; -*- coding: utf-8 -*-\n\n")
    fp.write('[Parameter]\n')
    fp.write(f'first-char={"".join(first_char)}\n')
    fp.write(f'total-char={"".join(total_char)}\n')
    fp.write('\n[Punctuation]')
    fp.write(f'{punc}')
    fp.write('\n[Table]\n')
    for key, value in sorted(table.items()):
        fp.write(f'{key} {value}\n')
        
