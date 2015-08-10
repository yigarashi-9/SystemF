# System F
TAPLの説明に沿って System F を実装する．修行の関係上，実装言語は Python となるが，Python のパーサーだけは使用に堪えないのでその部分だけHaskellで実装する．中間のフォーマットは JSON とする．
## System F とは
パラメータ多相を備えた単純型付きラムダ計算である．単純型付きラムダ計算に型抽象と型の具体化を導入する．

## 使い方
1. [stack](https://github.com/commercialhaskell/stack) をインストール
2. > stack build
3. > python main.py

(Python 3系での動作を想定しています)
