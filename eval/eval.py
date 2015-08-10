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
            res.accept(TmShift(-1))
            return res.accept(self)
        else:
            return TmApp(n_exp_l, n_exp_r)

    def tmtyabs(self, node):
        return node

    def tmtyapp(self, node):
        n_tm = node.tm.accept(self)
        if isinstance(n_tm, TmTyAbs):
            n_ty = node.ty.accept(TyVarShift(1))
            res = n_tm.body.accept(TySubst(n_ty))
            res.accept(TyShift(-1))
            return res.accept(self)
        else:
            return TmTyApp(n_tm, node.ty)


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


class TySubst(Subst):
    """型代入のために項を走査する visitor"""
    def tmvar(self, node):
        return node

    def tmabs(self, node):
        res = node.ty.accept(TyVarSubst(0, self.src))
        return TmAbs(node.var, res, node.body.accept(self))

    def tmtyapp(self, node):
        res = node.ty.accept(TyVarSubst(0, self.src))
        return TmTyApp(node.tm.accept(self), res)


class TyShift(Shift):
    """型代入に必要なインデックスのシフトを行う visitor"""
    def tmvar(self, node):
        pass

    def tmabs(self, node):
        node.ty = node.ty.accept(TyVarShift(self.shamt))
        node.body.accept(self)

    def tmtyapp(self, node):
        node.ty = node.ty.accept(TyVarShift(self.shamt))
        node.tm.accept(self)
