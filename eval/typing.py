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
        if tycond == TyBool():
            return join(tytrue, tyfalse)
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
        if isinstance(ty_l, TyArr) and subtype(ty_r, ty_l.left):
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


def subtype(ty_s, ty_t):
    """ty_s が ty_t の部分型であるかどうかをチェックする"""
    if isinstance(ty_t, TyTop):
        return True
    elif isinstance(ty_s, TyBool) and isinstance(ty_t, TyBool):
        return True
    elif isinstance(ty_s, TyArr) and isinstance(ty_t, TyArr):
        return subtype(ty_t.left, ty_s.left) and subtype(ty_s.right, ty_t.right)
    elif isinstance(ty_s, TyRcd) and isinstance(ty_t, TyRcd):
        return all([k in ty_s.rcd and subtype(ty_s.rcd[k], v) \
                    for k, v in ty_t.rcd.items()])
    else:
        return False


def join(ty_s, ty_t):
    """結びを計算する
       アルゴリズムは TaPL 演習 16.3.2 による
    """
    if isinstance(ty_s, TyBool) and isinstance(ty_t, TyBool):
        return TyBool()
    elif isinstance(ty_s, TyArr) and isinstance(ty_t, TyArr):
        left = meet(ty_s.left, ty_t.left)
        if left is None:
            return TyTop()
        else:
            return TyArr(left, join(ty_s.right, ty_t.right))
    elif isinstance(ty_s, TyRcd) and isinstance(ty_t, TyRcd):
        keys = set(ty_s.rcd.keys()) & set(ty_t.rcd.keys())
        return TyRcd({ k:join(ty_s.rcd[k], ty_t.rcd[k]) for k in keys })
    else:
        return TyTop()

def meet(ty_s, ty_t):
    """交わりを計算する"""
    if isinstance(ty_s, TyTop) or isinstance(ty_t, TyTop):
        return TyTop()
    elif isinstance(ty_s, TyBool) and isinstance(ty_t, TyBool):
        return TyBool()
    elif isinstance(ty_s, TyArr) and isinstance(ty_t, TyArr):
        right = meet(ty_s.right, ty_t.right)
        if right is None:
            return None
        else:
            return TyArr(join(ty_s.left, ty_t.left), right)
    elif isinstance(ty_s, TyRcd) and isinstance(ty_t, TyRcd):
        keys = set(ty_s.rcd.keys()) | set(ty_t.rcd.keys())
        res  = {}
        for k in keys:
            if k in ty_s.rcd and k in ty_t.rcd:
                res[k] = meet(ty_s.rcd[k], ty_t.rcd[k])
            elif k in ty_s.rcd:
                res[k] = ty_s.rcd[k]
            elif k in ty_t.rcd:
                res[k] = ty_t.rcd[k]
            else:
                return None
        return res
    else:
        return None


class TypingError(Exception):
    def __init__(self, tag):
        self.tag = tag

    def __str__(self):
        return str(self.tag)
