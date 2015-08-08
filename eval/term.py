# -*- coding: utf-8 -*-
from .node import *

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
