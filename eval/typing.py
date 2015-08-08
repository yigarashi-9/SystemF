# -*- coding: utf-8 -*-
from eval.ty import *
from eval.term import *

class Typing(object):
    """型検査を行うための visitor"""
    def __init__(self):
        self.table = []

    def tmtrue(self, node):
        return TyBool()

    def tmfalse(self, node):
        return TyBool()

    def tmif(self, node):
        tycond  = node.cond.accept(self)
        tytrue  = node.true.accept(self)
        tyfalse = node.false.accept(self)
        if tycond == TyBool() and tytrue == tyfalse:
            return tytrue
        else:
            raise TypingError("if")

    def tmvar(self, node):
        if len(self.table) > node.index:
            return self.table[node.index]
        else:
            raise TypingError(node.char)

    def tmabs(self, node):
        self.table.append(node.ty)
        return TyArr(node.ty, node.body.accept(self))

    def tmapp(self, node):
        ty_l = node.exp_l.accept(self)
        ty_r = node.exp_r.accept(self)
        if isinstance(ty_l, TyArr) and ty_l.left == ty_r:
            return ty_l.right
        else:
            raise TypingError("app")


class TypingError(Exception):
    def __init__(self, tag):
        self.tag = tag

    def __str__(self):
        return str(self.tag)
