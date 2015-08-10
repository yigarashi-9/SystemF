# -*- coding: utf-8 -*-
import unittest
from eval.execute import systemf_eval
from eval.ty import *
from eval.term import *

class TestRepl(unittest.TestCase):
    def test_bool(self):
        self.assertTrue(systemf_eval("true"), (TmTrue(), TyBool()))
        self.assertTrue(systemf_eval("false"), (TmFalse(), TyBool()))
        self.assertTrue(systemf_eval("(\\x:Bool x)"),
                        (TmAbs('x', TyBool(), TmVar(0, 'x')),
                         TyArr(TyBool(), TyBool)))
        self.assertTrue(systemf_eval("if true then false else true"),
                        (TmFalse(), TyBool()))
        self.assertTrue(systemf_eval("(\\x:Bool x) true"),
                        (TmTrue(), TyBool()))
        self.assertTrue(systemf_eval("(\\x:Bool->Bool x true) (\\x:Bool x)"),
                        (TmTrue(), TyBool()))
        self.assertTrue(systemf_eval("(\\x:Bool->Bool (\y:Bool x y)) (\\x:Bool x)"),
                        (TmAbs('y', TyBool(), TmApp(TmAbs('x', TyBool(), TyVar(1, 'x')),
                                                    TyVar(0, 'y'))),
                         TyArr(TyBool(), TyBool())))

        longcase1 = "(\\x:Bool->Bool->Bool x (\\y:Bool y)) (\\z:Bool->Bool z true)"
        self.assertTrue(systemf_eval(longcase1), (TmTrue(), TyBool()))

        longcase2 = "(\\x:Bool->Bool  (\\y:Bool x y) true) (\\x:Bool (\\y:Bool x) x)"
        self.assertTrue(systemf_eval(longcase2), (TmTrue(), TyBool()))

        longcase3 = "(\\x:Bool->Bool (\\y:Bool (\\z:Bool x y))) (\\x:Bool x) true true"
        self.assertTrue(systemf_eval(longcase3), (TmTrue(), TyBool()))


    def test_polymorphism(self):
        self.assertTrue(systemf_eval("(\\X (\\x:X x)) [Bool] true"),
                        (TmTrue(), TyBool()))
