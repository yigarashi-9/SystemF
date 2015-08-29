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
            src = node.ty.accept(TyVarShift(1))
            return ty_l.ty.accept(TyVarSubst(self.depth, src)).accept(TyVarShift(-1))
        else:
            TypingError('tyapp')

    def tmrcd(self, node):
        return TyRcd({ k:v.accept(self) for k, v in node.rcd.items()})

    def tmproj(self, node):
        ty_rcd = node.tmrcd.accept(self)
        if isinstance(ty_rcd, TyRcd) and node.label in ty_rcd.rcd:
            return ty_rcd.rcd[node.label]
        else:
            TypingError('tmproj')


class TypingError(Exception):
    def __init__(self, tag):
        self.tag = tag

    def __str__(self):
        return str(self.tag)
