# -*- coding: utf-8 -*-
import copy
from .node import *

class TyArr(Node):
    def __init__(self, left, right):
        self.left = left
        self.right = right

    def __str__(self):
        return str(self.left) + " -> " + str(self.right)

    def __eq__(self, other):
        if isinstance(other, TyArr):
            return self.left == other.left and self.right == other.right
        else:
            return False


class TyBool(Node):
    def __str__(self):
        return "Bool"

    def __eq__(self, other):
        return isinstance(other, TyBool)


class TyVar(Node):
    def __init__(self, var, index):
        self.var = var
        self.index = index

    def __str__(self):
        return str(self.var) + ":" + str(self.index)

    def __eq__(self, other):
        return isinstance(other, TyVar) and self.index == other.index


class TyAll(Node):
    def __init__(self, parm, ty):
        self.parm = parm
        self.ty = ty

    def __eq__(self, other):
        return isinstance(other, TyAll) and self.ty == other.ty

    def __str__(self):
        return "forall " + str(self.parm) + "." + str(self.ty)


class TyVarManip(object):
    """型変数を操作するための acceptor"""
    def tybool(self, ty):
        return TyBool()

    def tyarr(self, ty):
        n_ty_l = ty.left.accept(self)
        n_ty_r = ty.right.accept(self)
        return TyArr(n_ty_l, n_ty_r)

    def tyall(self, ty):
        n_ty = ty.ty.accept(self)
        return TyAll(ty.parm, n_ty)


class TyVarSubst(TyVarManip):
    """型代入で変数を置き換えるための visitor"""
    def __init__(self, depth, src):
        self.depth = depth
        self.src = src

    def tyvar(self, ty):
        """項の評価と違って，型検査は抽象の内側から評価が始まる場合もあるので
           現在の深さを記録して代入するべきインデックスを追いかける
        """
        if ty.index == self.depth:
            return copy.deepcopy(self.src)
        else:
            return copy.deepcopy(ty)


class TyVarShift(TyVarManip):
    """型代入時に型の中のパラメータをシフトするための visitor"""
    def __init__(self, shamt):
        self.shamt = shamt

    def tyvar(self, ty):
        n_ty = copy.deepcopy(ty)
        n_ty.index += self.shamt
        return n_ty
