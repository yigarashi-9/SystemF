# -*- coding: utf-8 -*-
from .ty import *
from .term import *

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
            shifted = n_exp_r.accept(TmShift(1))
            return n_exp_l.body.accept(TmSubst(shifted)).accept(TmShift(-1)).accept(self)
        else:
            return TmApp(n_exp_l, n_exp_r)

    def tmtyabs(self, node):
        return node

    def tmtyapp(self, node):
        n_tm = node.tm.accept(self)
        if isinstance(n_tm, TmTyAbs):
            n_ty = node.ty.accept(TyVarShift(1))
            return n_tm.body.accept(TySubst(n_ty)).accept(TyShift(-1)).accept(self)
        else:
            return TmTyApp(n_tm, node.ty)

    def tmrcd(self, node):
        return TmRcd({ k:v.accept(self) for k, v in node.rcd.items() })

    def tmproj(self, node):
        n_rcd = node.tmrcd.accept(self)
        if isinstance(n_rcd, TmRcd):
            return n_rcd.rcd[node.label]
        else:
            return TmProj(n_rcd, node.label)
