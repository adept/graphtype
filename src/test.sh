#!/bin/bash
runhaskell GraphType.hs Organization ../example/*.hs
dot -T png -o output.png output.dot
feh -FZ output.png
