# -*- coding: utf-8 -*-
import copy
from .node import *
from .ty   import *

class TmTrue(Node):
    def __str__(self):
        return "true"


class TmFalse(Node):
    def __str__(self):
        return "false"


class TmIf(Node):
    def __init__(self, cond, true, false):
        self.cond  = cond
        self.true  = true
        self.false = false

    def __str__(self):
        return "if " + str(self.cond) + \
               " then " + str(self.true) + \
               " else " + str(self.false)


class TmVar(Node):
    def __init__(self, index, char):
        self.index = index
        self.char  = char

    def __str__(self):
        return str(self.index) + ":" + self.char


class TmAbs(Node):
    def __init__(self, var, ty, body):
        self.var  = var
        self.ty   = ty
        self.body = body

    def __str__(self):
        return "(\\" + self.var + ":" + str(self.ty) + " " + str(self.body) + ")"


class TmApp(Node):
    def __init__(self, exp_l, exp_r):
        self.exp_l = exp_l
        self.exp_r = exp_r

    def __str__(self):
        return str(self.exp_l) + " " + str(self.exp_r)


class TmTyAbs(Node):
    def __init__(self, var, body):
        self.var = var
        self.body = body

    def __str__(self):
        return "(\\" + self.var + " " + str(self.body) + ")"


class TmTyApp(Node):
    def __init__(self, tm, ty):
        self.tm = tm
        self.ty = ty

    def __str__(self):
        return str(self.tm) + " [" + str(self.ty) + "]"


class TmManip(object):
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


class TmSubst(TmManip):
    """値の代入を行う visitor"""
    def __init__(self, src):
        self.src = src

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


class TmShift(TmManip):
    """値の代入に必要なシフト操作を行う visitor"""
    def __init__(self, shamt):
        self.shamt = shamt

    def tmvar(self, node):
        node.index += self.shamt
        return node

    def tmabs(self, node):
        n_body = node.body.accept(self)
        return TmAbs(node.var, node.ty, n_body)

    def tmtyapp(self, node):
        n_tm = node.tm.accept(self)
        return TmTyApp(n_tm, node.ty)


class TySubst(TmManip):
    """型代入のために項を走査する visitor"""
    def __init__(self, src):
        self.src = src

    def tmvar(self, node):
        return node

    def tmabs(self, node):
        res = node.ty.accept(TyVarSubst(0, self.src))
        return TmAbs(node.var, res, node.body.accept(self))

    def tmtyapp(self, node):
        res = node.ty.accept(TyVarSubst(0, self.src))
        return TmTyApp(node.tm.accept(self), res)


class TyShift(TmManip):
    """型代入に必要なインデックスのシフトを行う visitor"""
    def __init__(self, shamt):
        self.shamt = shamt

    def tmvar(self, node):
        return node

    def tmabs(self, node):
        n_ty   = node.ty.accept(TyVarShift(self.shamt))
        n_body = node.body.accept(self)
        return TmAbs(node.var, n_ty, n_body)

    def tmtyapp(self, node):
        n_ty = node.ty.accept(TyVarShift(self.shamt))
        n_tm = node.tm.accept(self)
        return TmTyApp(n_tm, n_ty)
