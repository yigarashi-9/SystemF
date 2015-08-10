# -*- coding: utf-8 -*-
import subprocess
import json
from . import convert
from . import typing
from . import eval

def systemf_eval(expr):
    json_txt = subprocess.check_output(['stack', 'exec', 'parse', expr])
    ast_dict = json.loads(json_txt.decode(('utf-8')))
    ast = convert.term(ast_dict)
    ty  = ast.accept(typing.Typing())
    res = ast.accept(eval.Eval())
    return (ty, res)
