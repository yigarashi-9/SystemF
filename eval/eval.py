# -*- coding: utf-8 -*-
import copy
from eval.ty import *
from eval.term import *

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
            res = n_exp_l.body.accept(Subst(n_exp_r))
            res.accept(Shift(-1))
            return res.accept(self)
        else:
            return TmApp(n_exp_l, n_exp_r)


class Subst(object):
    """代入を行う visitor．
       項を全て組み直している．
    """
    def __init__(self, src):
        self.src = src

    def tmtrue(self, node):
        return TmTrue()

    def tmfalse(self, node):
        return TmFalse()

    def tmif(self, node):
        n_cond = node.cond.accept(self)
        n_true = node.true.accept(self)
        n_false = node.false.accept(self)
        return TmIf(n_cond, n_true, n_false)

    def tmvar(self, node):
        if node.index == 0:
            return copy.deepcopy(self.src)
        else:
            return node

    def tmabs(self, node):
        self.src.accept(Shift(1))
        n_body = node.body.accept(self)
        self.src.accept(Shift(-1))
        return TmAbs(node.var, node.ty, n_body)

    def tmapp(self, node):
        n_exp_l = node.exp_l.accept(self)
        n_exp_r = node.exp_r.accept(self)
        return TmApp(n_exp_l, n_exp_r)


class Shift(object):
    """代入時に必要なシフト操作を行う visitor
       de Bruijin レベルを使っているので単純に全てシフトすれば良い
    """
    def __init__(self, shamt):
        self.shamt = shamt

    def tmtrue(self, node):
        pass

    def tmfalse(self, node):
        pass

    def tmif(self, node):
        node.cond.accept(self)
        node.true.accept(self)
        node.false.accept(self)

    def tmvar(self, node):
        node.index += self.shamt

    def tmabs(self, node):
        node.body.accept(self)

    def tmapp(self, node):
        node.exp_l.accept(self)
        node.exp_r.accept(self)
