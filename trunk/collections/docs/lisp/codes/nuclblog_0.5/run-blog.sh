#!/bin/sh
screen -D -m -S nuclblog sbcl --eval "(load \"ext-blog/wrap-run.lisp\")" --dynamic-space 90
