# -*- coding: utf-8 -*-
class Node(object):
    """ASTノードの抽象クラス"""

    def accept(self, visitor):
        """accept 側のノードに応じて， visitor 側のメソッドが選択される"""
        return self._accept(self.__class__, visitor)

    def _accept(self, klass, visitor):
        method = getattr(visitor, klass.__name__.lower(), None)
        return method(self)

def dict_eq(l, r):
    f = lambda v: v[0]
    dictl = sorted(l.items(), key=f)
    dictr = sorted(r.items(), key=f)
    return len(dictl) == len(dictr) and \
           all([vl[0] == vr[0] and vl[1] == vr[1] for (vl, vr) in zip(dictl, dictr)])
