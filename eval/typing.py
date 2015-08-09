# -*- coding: utf-8 -*-
from .ty import *
from .term import *

class Typing(object):
    """型検査を行うための visitor"""
    def __init__(self):
        self.table = []
        self.depth = 0

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
        res = TyArr(node.ty, node.body.accept(self))
        self.table = self.table[:-1]
        return res

    def tmapp(self, node):
        ty_l = node.exp_l.accept(self)
        ty_r = node.exp_r.accept(self)
        if isinstance(ty_l, TyArr) and ty_l.left == ty_r:
            return ty_l.right
        else:
            raise TypingError("app")

    def tmtyabs(self, node):
        self.depth += 1
        res = node.body.accept(self)
        self.depth -= 1
        return TyAll(node.var, res)

    def tmtyapp(self, node):
        ty_l = node.tm.accept(self)
        if isinstance(ty_l, TyAll):
            src = node.ty.accept(Update(1))
            n_ty_l = ty_l.ty.accept(Replace(self.depth, src))
            return n_ty_l.accept(Update(-1))
        else:
            TypingError('tyapp')


class TypingError(Exception):
    def __init__(self, tag):
        self.tag = tag

    def __str__(self):
        return str(self.tag)
