#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Filename: generate_dict.py
# Datetime: Mon Feb 17 11:00:44 2025
# Software: Emacs
# Author:   Gaeric

'''
1. 根据8105规范得出全部简码
2. 字频依据为北大中文19亿字频语料库
3. 要求尽可能填充所有编码位
4. 如果遇到重码，需要判断该重码所在位置，如果字频为前9000，则计入字，否则不记入该字
5. 最终输出的简码数据与8105.dict一致
'''

import copy

OPTIMIZE_RANGE = 4500


def get_freq_list():
    freq_list = []
    with open('freq.txt') as fp:
        data = fp.readlines()
        # skip first line and last line
        for line in data[1::]:
            line_split = line.split("\t")
            if len(line_split) != 5:
                continue
            freq_list.append(
                {'character': line_split[1],
                 'total_rate': float(line_split[-1].strip('\n'))})
    return freq_list


def get_8105_dict():
    dict_8105 = {}
    with open('8105.dict.yaml') as fp:
        data = fp.readlines()
        for line in data:
            line_split = line.split("\t")
            if len(line_split) != 2:
                continue
            dict_8105[line_split[0]] = line_split[1].strip()
    return dict_8105


def get_pre_list():
    pre_dict = []
    with open('code.txt') as fp:
        data = fp.readlines()
    for line in data:
        line_split = line.split('\t')
        pre_dict.append({'character': line_split[0],
                         'code': line_split[1],
                         'repeat': line_split[2]
                         })
    return pre_dict


def sort_list_with_freq(pre_list, freq_list):
    sorted_list = []
    freq_index = {chara['character']: index for index,
                  chara in enumerate(freq_list)}
    sorted_list = sorted(pre_list, key=lambda x: freq_index.get(
        x['character'], float('inf')))
    return sorted_list


def extend_table(sorted_list, sample_dict):
    optimize_dict = {sample_dict.get(item): [item] for item in sample_dict}
    for item in sorted_list:
        character = item.get('character')
        code = item.get('code')

        if sample_dict.get(character):
            continue
        else:
            if not optimize_dict.get(code):
                optimize_dict[code] = [character]
                continue

            if len(optimize_dict) < 9000:
                optimize_dict[code].append(character)

    table_list = []
    for code, characters in optimize_dict.items():
        for character in characters:
            table_list.append({'character': character, 'code': code})

    return sort_list_with_freq(table_list, freq_list)


def optimize_table(sorted_list, freq_list, sample_dict):
    '''对前OPTIMIZE_RANGE字以去重为目标进行简码化
    1. 对于重码单字，生成简码减少重复
    2. 第1步完成后，对全码进行扫描并生成单字简码表
    '''
    pre_optimize = {}

    for item in sorted_list[0:OPTIMIZE_RANGE]:
        code = item.get('code')
        character = item.get('character')
        if (pre_optimize.get(code)):
            pre_optimize[code].append(character)
        else:
            pre_optimize[code] = [character]

    optimize_dict, single_list = optimize_dup(pre_optimize)

    single_list.extend(sorted_list[OPTIMIZE_RANGE::])
    total_dict = optimize_codelen(single_list, optimize_dict)

    table_list = []
    for code, characters in total_dict.items():
        for character in characters:
            table_list.append({'character': character, 'code': code})

    return sort_list_with_freq(table_list, freq_list)


def optimize_dup(pre_optimize):
    dupdict = {}
    single_list = []
    # optimize_list = []
    for code, character in pre_optimize.items():
        if len(character) > 1:
            dupdict[code] = character
        else:
            single_list.append(
                {'character': character[0], 'code': code, 'repeat': 0})

    optimize_dict = {}
    for code, characters in dupdict.items():
        clear_code = code[0:-1]
        if optimize_dict.get(clear_code):
            continue
        optimize_dict[clear_code] = characters[0:1]
        optimize_dict[code] = characters[1:]

    return optimize_dict, single_list


def optimize_codelen(code_list, optimize_dict):
    for index in range(len(code_list)):
        code = code_list[index].get('code')
        character = code_list[index].get('character')

        clear_code = code[0:-1]
        if not optimize_dict.get(clear_code):
            optimize_dict[clear_code] = [character]
            continue

        if not optimize_dict.get(code):
            optimize_dict[code] = [character]
            continue

        if index < 9000 - OPTIMIZE_RANGE:
            optimize_dict[code].append(character)

    return optimize_dict


def diff_8105_dict(table_list, dict_8105):
    table_dict = {item.get('character'): item.get('code')
                  for item in table_list}
    for item in dict_8105:
        code = table_dict.get(item)
        code_8105 = dict_8105.get(item)
        # if code != code_8105:
        if code and code != code_8105:
            print(f'{item}: {code} not match 8105 {code_8105}')


def get_sample_dict():
    sample_dict = {}
    with open('sample.table.txt') as fp:
        data = fp.readlines()
        for line in data:
            line_split = line.split("\t")
            sample_dict[line_split[0]] = line_split[1].strip()
    return sample_dict


def write_sample_table(dict_8105):
    with open('sample.table.txt', 'w', encoding='utf-8') as fp:
        for item in dict_8105:
            code = dict_8105.get(item)
            if len(code) == 2:
                fp.write(f'{item}\t{code}\n')


def write_table(table_list):
    with open("dict.table.txt", 'w', encoding='utf-8') as fp:
        for item in table_list:
            fp.write(f'{item.get("character")}\t{item.get("code")}\n')


if __name__ == '__main__':
    freq_list = get_freq_list()
    dict_8105 = get_8105_dict()
    # write_simple_table(dict_8105)
    sample_dict = get_sample_dict()
    pre_list = get_pre_list()
    sorted_list = sort_list_with_freq(pre_list, freq_list)
    # table_list = optimize_table(sorted_list, freq_list, sample_dict)
    table_list = extend_table(sorted_list, sample_dict)
    diff_8105_dict(table_list, dict_8105)
    write_table(table_list)
