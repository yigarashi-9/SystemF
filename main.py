
import subprocess
import json
from eval import convert
from eval import typing
from eval import eval

def driver_loop():
    while(1):
        try:
            expr = input('> ')
            json_txt = subprocess.check_output(['stack', 'exec', 'parse', expr])
            ast_dict = json.loads(json_txt.decode(('utf-8')))
            ast = convert.term(ast_dict)
            ty  = ast.accept(typing.Typing())
            res = ast.accept(eval.Eval())
            print("{0} : {1}".format(res, ty))
        except subprocess.CalledProcessError as err:
            pass
        except typing.TypingError as err:
            print("type error : " + str(err.args))
        except KeyboardInterrupt:
            print("exit")
            break


if __name__ == "__main__":
    driver_loop()
