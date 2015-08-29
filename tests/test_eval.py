# -*- coding: utf-8 -*-
import unittest
from eval.execute import systemf_eval
from eval.ty import *
from eval.term import *

class TestRepl(unittest.TestCase):

    def test_bool(self):
        self.assertEqual(systemf_eval("true"), (TyBool(), TmTrue()))
        self.assertEqual(systemf_eval("false"), (TyBool(), TmFalse()))
        self.assertEqual(systemf_eval("(\\x:Bool x)"),
                        (TyArr(TyBool(), TyBool()),
                         TmAbs('x', TyBool(), TmVar(0, 'x'))))
        self.assertEqual(systemf_eval("if true then false else true"),
                        (TyBool(), TmFalse()))
        self.assertEqual(systemf_eval("(\\x:Bool x) true"),
                        (TyBool(), TmTrue()))
        self.assertEqual(systemf_eval("(\\x:Bool->Bool x true) (\\x:Bool x)"),
                        (TyBool(), TmTrue()))
        self.assertEqual(systemf_eval("(\\x:Bool->Bool (\y:Bool x y)) (\\x:Bool x)"),
                        (TyArr(TyBool(), TyBool()),
                         TmAbs('y', TyBool(), TmApp(TmAbs('x', TyBool(), TmVar(1, 'x')),
                                                    TmVar(0, 'y')))))

        longcase1 = "(\\x:Bool->Bool->Bool x (\\y:Bool y)) (\\z:Bool->Bool z true)"
        self.assertEqual(systemf_eval(longcase1), (TyBool(), TmTrue()))

        longcase2 = "(\\x:Bool->Bool  (\\y:Bool x y) true) (\\x:Bool (\\y:Bool x) x)"
        self.assertEqual(systemf_eval(longcase2), (TyBool(), TmTrue()))

        longcase3 = "(\\x:Bool->Bool (\\y:Bool (\\z:Bool x y))) (\\x:Bool x) true true"
        self.assertEqual(systemf_eval(longcase3), (TyBool(), TmTrue()))


    def test_polymorphism(self):
        self.assertEqual(systemf_eval("(\\X (\\x:X x)) [Bool] true"),
                        (TyBool(), TmTrue()))
        self.assertEqual(systemf_eval("(\\X (\\Y (\\x:Y x)) [X->X]) [Bool] (\\x:Bool x)"),
                        (TyArr(TyBool(), TyBool()), TmAbs('x', TyBool(), TmVar(0, 'x'))))

    def test_record(self):
        self.assertEqual(systemf_eval("{x=true, y=\\a:Bool a}.y true"),
                         (TyBool(), TmTrue()))
        self.assertEqual(systemf_eval("{x=true, y={z=\\a:Bool a}}.y.z true"),
                         (TyBool(), TmTrue()))
        self.assertEqual(systemf_eval("{x=true, y=\\a:Bool a}"),
                         (TyRcd({'x':TyBool(), 'y':TyArr(TyBool(), TyBool())}),
                          TmRcd({'x':TmTrue(), 'y':TmAbs('a', TyBool(), TmVar(0, 'a'))})))
        self.assertEqual(systemf_eval("(\\x:{y:Bool} x.y) {y=true}"),
                         (TyBool(), TmTrue()))
