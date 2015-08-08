# -*- coding: utf-8 -*-
class Node(object):
    """ASTノードの抽象クラス"""

    def accept(self, visitor):
        """accept 側のノードに応じて， visitor 側のメソッドが選択される"""
        return self._accept(self.__class__, visitor)

    def _accept(self, klass, visitor):
        method = getattr(visitor, klass.__name__.lower(), None)
        return method(self)


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
