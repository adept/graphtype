#!/bin/bash
runhaskell ../src/GraphType.hs "$@" Organization ../example/*.hs
dot -T png -o output.png output.dot
