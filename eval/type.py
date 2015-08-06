
class TyArr(object):
    def __init__(self, left, right):
        self.left = left
        self.right = right

    def __eq__(self, other):
        if other.__class__.__name__ == "TyArr":
            return self.left == other.left and self.right == self.right
        else:
            return False


class TyBool(object):
    def __eq__(self, other):
        return other.__class__.__name__ == "TyBool"
