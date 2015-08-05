
import subprocess
import json

def driver_loop():
    while(1):
        expr = input('> ')
        try:
            json_txt = subprocess.check_output(['stack', 'exec', 'parse', expr])
            ast      = json.loads(json_txt.decode(('utf-8')))
            print(ast)
        except CalledProcessError as err:
            print(err.message)

if __name__ == "__main__":
    driver_loop()
