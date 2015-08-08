# -*- coding: utf-8 -*-
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
