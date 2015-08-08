# -*- coding: utf-8 -*-
class Shift(object):
    """代入時に必要なシフト操作を行う抽象的な visitor．
       de Bruijin レベルを使っているので単純に全てシフトすれば良い．
       値代入，型代入の両方に派生する．
    """
    def __init__(self, shamt):
        self.shamt = shamt

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
        pass

    def tmfalse(self, node):
        pass

    def tmif(self, node):
        node.cond.accept(self)
        node.true.accept(self)
        node.false.accept(self)

    def tmapp(self, node):
        node.exp_l.accept(self)
        node.exp_r.accept(self)

    def tmtyabs(self, node):
        node.body.accept(self)
