#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Filename: dict_parser.py
# Datetime: Mon Aug  7 22:05:53 2023
# Software: Emacs
# Author:   Gaeric

import datetime
import re


sb_regex = re.compile(f'.[aeuio]{1}')
sbb_regex = re.compile(f'.[aeuio]{2}')
CONCISE_KIND = ['sj', 'sb', 'sp', 'sbb', 'spb']

def get_encode_kind(encode):

    if len(encode) == 1:
        return "sj"
    elif len(encode) == 2:
        if sb_regex.match(encode):
            return 'sb'
        else:
            return 'sp'
    elif len(encode) == 3:
        if sbb_regex.match(encode):
            return 'sbb'
        else:
            return 'spb'

    return 'full'


class Item:
    """{chara}\t{encode}\t{freq}"""
    def __init__(self, encode, chara, freq = ""):
        self.chara = chara
        self.encode = encode
        self.freq = freq

    def display(self):
        if self.freq:
            return f'{self.chara}\t{self.encode}\t{self.freq}\n'
        else:
            return f'{self.chara}\t{self.encode}\n'


class Table:
    def __init__(self, name, comment):
        self.name = name
        self.comment = comment
        self.version = datetime.datetime.now().strftime("%Y%m%d%H%M")
        self.table = {}
        self.table['full'] = []
        for kind in CONCISE_KIND:
            self.table[kind] = []

    def build(self, content):
        for line in content:
            (encode, chara) = parse_line(line)
            item = Item(encode, chara)
            kind = get_encode_kind(encode)
            if kind != 'full':
                self.table[kind].append(item)
            self.table['full'].append(item)
        self.clear()

    def clear(self):
        for item in self.table['full']:
            for kind in CONCISE_KIND:
                for concise_item in self.table[kind]:
                    # chara same, judge startswith and len
                    if item.chara == concise_item.chara and \
                       item.encode != concise_item.encode and \
                           item.encode.startswith(concise_item.encode):
                        # there only one full encode, remove concise_item for performance
                        self.table[kind].remove(concise_item)
                        self.table['full'].remove(item)
                            
    def write(self, filepath):
        with open(filepath, 'w', encoding='utf-8') as fp:
            fp.write(f'# Rime dict \n# encoding: utf-8\n# {self.comment}\n\n')
            fp.write(f'---\nname: {self.name}\nversion: {self.version}\n')
            fp.write('sort: original\n')
            fp.write('...\n')
            for item in self.table['full']:
                fp.write(item.display())
            

def parse_line(line):
    line_content = line.split()
    chara = line_content[0].strip()
    encode = line_content[1].strip()
    return (encode, chara)
        

if __name__ == "__main__":
    with open('./data/sbfddict_20230805.txt') as fp:
        content = fp.readlines()

    table = Table('starfire_fd', "星火飞单")
    table.build(content)
    table.write('./starfire_fd.dict.yaml')
