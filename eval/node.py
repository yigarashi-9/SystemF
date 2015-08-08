# -*- coding: utf-8 -*-
class Node(object):
    """ASTノードの抽象クラス"""

    def accept(self, visitor):
        """accept 側のノードに応じて， visitor 側のメソッドが選択される"""
        return self._accept(self.__class__, visitor)

    def _accept(self, klass, visitor):
        method = getattr(visitor, klass.__name__.lower(), None)
        return method(self)
