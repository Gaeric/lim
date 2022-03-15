#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Filename: rimedict2quail.py
# Datetime: Mon Mar 14 15:59:30 2022
# Software: Emacs
# Author:   Gaeric


commnet = ''''''


class Table:
    def __init__(self, dict_file):
        self.table = {}
        with open(dict_file) as fp:
            content = fp.readlines()

        start = False
        first_char = set()
        total_char = set()
        for line in content:
            if start:
                line_content = line.split()
                chara = line_content[0]
                encode = line_content[1]
                if self.table.get(encode):
                    self.table[encode] += f' {chara}'
                else:
                    self.table[encode] = f'{chara}'
                    first_char.add(encode[0])
                total_char.update(list(encode))
            elif line.startswith('...'):
                start = True
        self.first_char = [x for x in first_char]
        self.total_char = [x for x in total_char]
        self.first_char.sort()
        self.total_char.sort()
