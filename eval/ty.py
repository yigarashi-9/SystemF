# -*- coding: utf-8 -*-
class TyArr(object):
    def __init__(self, left, right):
        self.left = left
        self.right = right

    def __str__(self):
        return str(self.left) + " -> " + str(self.right)

    def __eq__(self, other):
        if type(self) == type(other):
            return self.left == other.left and self.right == self.right
        else:
            return False


class TyBool(object):
    def __str__(self):
        return "Bool"

    def __eq__(self, other):
        return type(self) == type(other)
