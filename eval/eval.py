# -*- coding: utf-8 -*-
import copy
from .ty import *
from .term import *
from .shift import *
from .subst import *

class Eval(object):
    """項の評価を行う visitor"""
    def tmtrue(self, node):
        return node

    def tmfalse(self, node):
        return node

    def tmif(self, node):
        cond = node.cond.accept(self)
        if isinstance(cond, TmTrue):
            return node.true.accept(self)
        elif isinstance(cond, TmFalse):
            return node.false.accept(self)

    def tmvar(self, node):
        return node

    def tmabs(self, node):
        return node

    def tmapp(self, node):
        n_exp_l = node.exp_l.accept(self)
        n_exp_r = node.exp_r.accept(self)
        if isinstance(n_exp_l, TmAbs):
            n_exp_r.accept(TmShift(1))
            res = n_exp_l.body.accept(TmSubst(n_exp_r))
            res.accept(Shift(-1))
            return res.accept(self)
        else:
            return TmApp(n_exp_l, n_exp_r)


class TmSubst(Subst):
    """値の代入を行う visitor"""
    def tmvar(self, node):
        if node.index == 0:
            return copy.deepcopy(self.src)
        else:
            return node

    def tmabs(self, node):
        self.src.accept(TmShift(1))
        n_body = node.body.accept(self)
        self.src.accept(TmShift(-1))
        return TmAbs(node.var, node.ty, n_body)

    def tmtyapp(self, node):
        n_tm = node.tm.accept(self)
        return TmTyApp(n_tm, node.ty)


class TmShift(Shift):
    """値の代入に必要なシフト操作を行う visitor"""
    def tmvar(self, node):
        node.index += self.shamt

    def tmabs(self, node):
        node.body.accept(self)

    def tmtyapp(self, node):
        node.tm.accept(self)
