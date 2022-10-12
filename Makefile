.POSIX:
EMACS = emacs

compile: b4.elc

bitpack.elc: b4.el

clean:
	rm -f b4.elc

check: b4.elc
	$(EMACS) -batch -Q -L . -l b4.elc

.SUFFIXES: .el .elc
.el.elc:
	$(EMACS) -batch -Q -L . -f batch-byte-compile $<
