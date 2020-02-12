#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Filename: build_symbol.py
# Datetime: Mon Feb 10 14:11:23 2020
# Software: Emacs
# Author:   Gaeric

from yaml import load
from yaml import dump
from yaml import CLoader
from pprint import pprint

PAIRS = {}
PUNCS = {}
COUPLES = {}
COUPLES_SYMBOLS = {'<', '>', '[', ']', '{', '}', '(', ')'}


def get_shape():
    with open("./symbol.yaml") as fp:
        fd = fp.read()

    data = load(fd, Loader=CLoader)
    result = data.get('config_version')
    full_shape = data.get("punctuator").get('full_shape')
    return full_shape


def build_punc(full_shape):
    for key, item in full_shape.items():
        # print(type(item), end=' ')
        # symbol = key + " "
        if isinstance(item, dict):
            if (item.get('pair')):
                pair = item.get('pair')
                result = " ".join(pair)
                PAIRS[key] = result
            else:
                values = item.values()
                result = " ".join(values)
                PUNCS[key] = result

        if isinstance(item, str):
            PUNCS[key] = item

        if isinstance(item, list):
            result = " ".join(item)
            if key in COUPLES_SYMBOLS:
                COUPLES[key] = result
            else:
                PUNCS[key] = result


def write_symbol_file():
    with open('lim-symbol.txt', "w") as fp:
        fp.write(";; -*- coding: utf-8 -*-")
        fp.write("\n[PUNCS]\n")
        for key, item in PUNCS.items():
            fp.write(key + " " + item + '\n')

        fp.write("\n[PAIRS]\n")
        for key, item in PAIRS.items():
            fp.write(key + " " + item + '\n')

        fp.write("\n[COUPLES]\n")
        for key, item in COUPLES.items():
            fp.write(key + " " + item + "\n")


if __name__ == "__main__":
    full_shape = get_shape()
    build_punc(full_shape)
    pprint(PUNCS)
    pprint(PAIRS)
    pprint(COUPLES)
    write_symbol_file()

