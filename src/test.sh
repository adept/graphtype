#!/bin/bash
runhaskell GraphType.hs
dot -T png -o output.png output.dot
feh -FZ output.png
