app [main] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.15.0/SlwdbJ-3GR7uBWQo6zlmYWNYOxnvo8r6YABXD-45UOw.tar.br" }

## This is a comment for documentation, and includes a code block.
##
##     x = 2
##     expect x == 3

import pf.Stdout

birds = 3

iguanas = 2

total = Num.toStr (birds + iguanas)

main =
  dbg List.map [1, 2, 3] Num.isOdd
  Stdout.line! "There are $(total) animals."