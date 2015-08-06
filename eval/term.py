
class Node(object):
    """ASTノードの抽象クラス"""

    def accept(self, visitor):
        return self._accept(self.__class__, visitor)

    def _accept(self, klass, visitor):
        method = getattr(visitor, klass.__name__, None)
        return method(self)


class TmTrue(Node):
    pass


class TmFalse(Node):
    pass


class TmIf(Node):
    def __init__(self, cond, true, false):
        self.cond  = cond
        self.true  = true
        self.false = false


class TmVar(Node):
    def __init__(self, index, char):
        self.index = index
        self.char  = char


class TmAbs(Node):
    def __init__(self, var, ty, body):
        self.var  = var
        self.ty   = ty
        self.body = body


class TmApp(Node):
    def __init__(self, exp_l, exp_r):
        self.exp_l = exp_l
        self.exp_r = exp_r
