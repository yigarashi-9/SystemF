# -*- coding: utf-8 -*-
import copy
from .node import *
from .ty   import *

class TmTrue(Node):
    def __str__(self):
        return "true"

    def __eq__(self, other):
        return isinstance(other, TmTrue)

class TmFalse(Node):
    def __str__(self):
        return "false"

    def __eq__(self, other):
        return isinstance(other, TmFalse)

class TmIf(Node):
    def __init__(self, cond, true, false):
        self.cond  = cond
        self.true  = true
        self.false = false

    def __str__(self):
        return "if " + str(self.cond) + \
               " then " + str(self.true) + \
               " else " + str(self.false)

    def __eq__(self, other):
        return isinstance(other, TmIf) and self.cond == other.cond and \
               self.true == other.true and self.false == other.false

class TmVar(Node):
    def __init__(self, index, char):
        self.index = index
        self.char  = char

    def __str__(self):
        return self.char

    def __eq__(self, other):
        return isinstance(other, TmVar) and self.index == other.index

class TmAbs(Node):
    def __init__(self, var, ty, body):
        self.var  = var
        self.ty   = ty
        self.body = body

    def __str__(self):
        return "(\\" + self.var + ":" + str(self.ty) + " " + str(self.body) + ")"

    def __eq__(self, other):
        return isinstance(other, TmAbs) and \
               self.ty == other.ty and self.body == other.body

class TmApp(Node):
    def __init__(self, exp_l, exp_r):
        self.exp_l = exp_l
        self.exp_r = exp_r

    def __str__(self):
        return str(self.exp_l) + " " + str(self.exp_r)

    def __eq__(self, other):
        return isinstance(other, TmApp) and \
               self.exp_l == other.exp_l and self.exp_r == self.exp_r

class TmTyAbs(Node):
    def __init__(self, var, body):
        self.var = var
        self.body = body

    def __str__(self):
        return "(\\" + self.var + " " + str(self.body) + ")"

    def __eq__(self, other):
        return isinstance(other, TmTyAbs) and self.body == other.body

class TmTyApp(Node):
    def __init__(self, tm, ty):
        self.tm = tm
        self.ty = ty

    def __str__(self):
        return str(self.tm) + " [" + str(self.ty) + "]"

    def __eq__(self, other):
        return isinstance(other, TmTyApp) and \
               self.tm == other.tm and self.ty == other.tm

class TmRcd(Node):
    def __init__(self, rcd):
        self.rcd = rcd

    def __str__(self):
        body = ", ".join(map(lambda v: str(v[0]) + "=" + str(v[1]), self.rcd.items()))
        return "{" + body + "}"

    def __eq__(self, other):
        return isinstance(other, TmRcd) and dict_eq(self.rcd, other.rcd)

class TmProj(Node):
    def __init__(self, tmrcd, label):
        self.tmrcd = tmrcd
        self.label = label

    def __str__(self):
        return str(self.tmrcd) + "." + str(self.label)


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

    def tmrcd(self, node):
        return TmRcd({ k:v.accept(self) for k, v in node.rcd.items() })

    def tmproj(self, node):
        return TmProj(node.tmrcd.accept(self), node.label)


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
