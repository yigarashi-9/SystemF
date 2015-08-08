# -*- coding: utf-8 -*-
from .term import *

class Subst(object):
    """代入を行う抽象的な visitor．
       項を全て組み直している．
    """
    def __init__(self, src):
        self.src = src

    def tmvar(self, node):
        """派生クラスで実装"""
        pass

    def tmabs(self, node):
        """派生クラスで実装"""
        pass

    def tmtyapp(self, node):
        """派生クラスで実装"""
        pass

    def tmtrue(self, node):
        return TmTrue()

    def tmfalse(self, node):
        return TmFalse()

    def tmif(self, node):
        n_cond = node.cond.accept(self)
        n_true = node.true.accept(self)
        n_false = node.false.accept(self)
        return TmIf(n_cond, n_true, n_false)

    def tmapp(self, node):
        n_exp_l = node.exp_l.accept(self)
        n_exp_r = node.exp_r.accept(self)
        return TmApp(n_exp_l, n_exp_r)

    def tmtyabs(self, node):
        n_body = node.body.accept(self)
        return TmTyAbs(node.var, n_body)
