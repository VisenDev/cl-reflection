
.PHONY: main

main: main.lisp
	sbcl --disable-debugger --no-userinit --eval "(progn (load \"main.lisp\")(sb-ext:save-lisp-and-die \"main\" :toplevel #'main :executable t))"
