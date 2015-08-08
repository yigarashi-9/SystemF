# -*- coding: utf-8 -*-
import copy
from .ty import *
from .term import *
from .subst import *
from .shift import *

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


class TySubst(Subst):
    """型代入を行う visitor"""
    def tmvar(self, node):
        return node

    def tmabs(self, node):
        res = node.ty.accept(Replace((self.src)))
        return TmAbs(node.var, res, node.body.accept(self))

    def tmtyapp(self, node):
        res = node.ty.accept(Replace(self.src))
        return TmTyApp(node.tm.accept(self), res)


class TyShift(Shift):
    """型代入に必要な型のインデックスのシフトを行う visitor"""
    def tmvar(self, node):
        pass

    def tmabs(self, node):
        node.ty = node.ty.accept(Update(self.shamt))
        node.body.accept(self)

    def tmtyapp(self, node):
        node.ty = node.ty.accept(Update(self.shamt))
        node.tm.accept(self)


class TyManip(object):
    """型を操作するための acceptor"""
    def tybool(self, ty):
        return TyBool()

    def tyarr(self, ty):
        n_ty_l = ty.left.accept(self)
        n_ty_r = ty.right.accept(self)
        return TyArr(n_ty_l, n_ty_r)

    def tyall(self, ty):
        n_ty = ty.ty.accept(self)
        return TyAll(ty.parm, n_ty)


class Replace(TyManip):
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


class Update(TyManip):
    """型代入時に型の中のパラメータをシフトするための visitor"""
    def __init__(self, shamt):
        self.shamt = shamt

    def tyvar(self, ty):
        ty.index += self.shamt
        return ty


class TypingError(Exception):
    def __init__(self, tag):
        self.tag = tag

    def __str__(self):
        return str(self.tag)
