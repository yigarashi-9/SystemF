# -*- coding: utf-8 -*-
import unittest
from eval.execute import systemf_repl
from eval.ty import *
from eval.term import *

class TestRepl(unittest.TestCase):
    def test_systemf_repl_bool(self):
        self.assertTrue(systemf_repl("true"), (TmTrue(), TyBool()))
        self.assertTrue(systemf_repl("false"), (TmFalse(), TyBool()))
        self.assertTrue(systemf_repl("(\\x:Bool x)"),
                        (TmAbs('x', TyBool(), TmVar(0, 'x')),
                         TyArr(TyBool(), TyBool)))
        self.assertTrue(systemf_repl("if true then false else true"),
                        (TmFalse(), TyBool()))
        self.assertTrue(systemf_repl("(\\x:Bool x) true"),
                        (TmTrue(), TyBool()))
        self.assertTrue(systemf_repl("(\\x:Bool->Bool x true) (\\x:Bool x)"),
                        (TmTrue(), TyBool()))

        longcase1 = "(\\x:Bool->Bool->Bool x (\\y:Bool y)) (\\z:Bool->Bool z true)"
        self.assertTrue(systemf_repl(longcase1), (TmTrue(), TyBool()))

        longcase2 = "(\\x:Bool->Bool  (\\y:Bool x y) true) (\\x:Bool (\\y:Bool x) x)"
        self.assertTrue(systemf_repl(longcase2), (TmTrue(), TyBool()))
