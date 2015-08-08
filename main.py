# -*- coding: utf-8 -*-
import subprocess
from eval import typing
from eval import execute

def driver_loop():
    while(1):
        try:
            expr = input("> ")
            (res, ty) = execute.systemf_repl(expr)
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
