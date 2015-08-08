# -*- coding: utf-8 -*-
from .ty import *
from .term import *

def term(ast):
    """JSON 形式の AST をクラスに変換する"""
    tag = ast['tag']
    cnt = ast['contents']

    if tag == 'TmTrue':
        return TmTrue()
    elif tag == 'TmFalse':
        return TmFalse()
    elif tag == 'TmIf':
        return TmIf(*[term(x) for x in cnt])
    elif tag == 'TmVar':
        return TmVar(*ast['contents'])
    elif tag == 'TmAbs':
        return TmAbs(cnt[0], ty(cnt[1]), term(cnt[2]))
    elif tag == 'TmApp':
        return TmApp(term(cnt[0]), term(cnt[1]))


def ty(ast):
    """JSON 形式の AST の型部分を変換する"""
    tag = ast['tag']
    cnt = ast['contents']

    if tag == 'TyBool':
        return TyBool()
    elif tag == 'TyArr':
        return TyArr(ty(cnt[0]), ty(cnt[1]))
