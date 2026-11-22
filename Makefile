EMACS ?= emacs
BATCH = $(EMACS) -Q --batch -L .

SRCS = omnidiff-theme.el \
       omnidiff-engine.el \
       omnidiff-image.el \
       omnidiff-dual.el \
       omnidiff-render.el \
       omnidiff.el

.PHONY: all compile test test-src clean

all: test

compile:
	$(BATCH) --eval "(setq byte-compile-warnings t byte-compile-error-on-warn t)" -f batch-byte-compile $(SRCS)

test: compile
	$(BATCH) -L test -l ert -l omnidiff-test -f ert-run-tests-batch-and-exit

test-src:
	$(BATCH) --eval "(setq load-prefer-newer t)" -L test -l ert -l omnidiff-test -f ert-run-tests-batch-and-exit

clean:
	rm -f *.elc test/*.elc
