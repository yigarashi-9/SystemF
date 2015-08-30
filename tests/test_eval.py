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
        self.assertEqual(systemf_eval("(\\X (\\x:{y:X} x.y)) [Bool] {y=true}"),
                         (TyBool(), TmTrue()))

    def test_subtype(self):
        self.assertEqual(systemf_eval("(\\x:{y:Bool} x.y) {y=true, z=false}"),
                         (TyBool(), TmTrue()))
        self.assertEqual(systemf_eval("(\\x:Top x) true"),
                         (TyTop(), TmTrue()))

        longcase1 = "(\\x:{y:Bool} -> {z:Bool} x {y=true}) (\\x:Top {y=true, z=false})"
        self.assertEqual(systemf_eval(longcase1),
                         (TyRcd({'z':TyBool()}),
                          TmRcd({'y':TmTrue(), 'z':TmFalse()})))

    def test_join_meet(self):
        longcase1 = "if true then {x=true, y=false} else {y=true, z=true}"
        self.assertEqual(systemf_eval(longcase1),
                         (TyRcd({'y':TyBool()}),
                          TmRcd({'x':TmTrue(), 'y':TmFalse()})))

        longcase2 = "if true then (\\x:{y:Bool} {z=true}) \
                     else (\\x:{w:Bool} {a=true, z=x.w})"
        self.assertEqual(systemf_eval(longcase2),
                         (TyArr(TyRcd({'y':TyBool(), 'w':TyBool()}),
                                TyRcd({'z':TyBool()})),
                          TmAbs('x', TyRcd({'y':TyBool()}), TmRcd({'z':TmTrue()}))))

        longcase3 = "(\\x:{y:{z:Bool}} x.y.z) {y={z=true, w=false}}"
        self.assertEqual(systemf_eval(longcase3),
                         (TyBool(), TmTrue()))
