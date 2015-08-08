# -*- coding: utf-8 -*-
import subprocess
import json
from . import convert
from . import typing
from . import eval

def systemf_repl(expr):
    json_txt = subprocess.check_output(['stack', 'exec', 'parse', expr])
    ast_dict = json.loads(json_txt.decode(('utf-8')))
    ast = convert.term(ast_dict)
    print(ast)
    ty  = ast.accept(typing.Typing())
    print(ty)
    res = ast.accept(eval.Eval())
    return (ty, res)
