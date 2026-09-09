EMACS ?= emacs
ELFILES := t3-code-core.el t3-code-render.el t3-code-thread.el t3-code-dashboard.el t3-code.el
TESTFILES := test/t3-code-core-test.el test/t3-code-thread-test.el test/t3-code-dashboard-test.el test/t3-code-integration-test.el

.PHONY: check test compile clean

check: compile test

compile:
	$(EMACS) -Q --batch -L . -f batch-byte-compile $(ELFILES)

test:
	$(EMACS) -Q --batch -L . -L test \
		-l t3-code-core-test -l t3-code-thread-test -l t3-code-dashboard-test -l t3-code-integration-test \
		-f ert-run-tests-batch-and-exit

clean:
	rm -f *.elc test/*.elc
