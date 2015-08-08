# -*- coding: utf-8 -*-
class TyArr(object):
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


class TyBool(object):
    def __str__(self):
        return "Bool"

    def __eq__(self, other):
        return isinstance(other, TyBool)
