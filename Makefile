EMACS ?= emacs
ELFILES := t3-code-core.el t3-code-render.el t3-code-thread.el t3-code-dashboard.el t3-code.el
TESTFILES := test/t3-code-core-test.el test/t3-code-thread-test.el test/t3-code-dashboard-test.el test/t3-code-integration-test.el
T3CODE_DIR ?= ../t3code

.PHONY: check check-t3code test compile clean

check: compile test

# Opt-in compatibility gate for the adjacent T3 source checkout.
check-t3code:
	cd $(T3CODE_DIR) && ./node_modules/.bin/vitest run apps/server/src/cli/client.test.ts
	T3CODE_DIR=$(abspath $(T3CODE_DIR)) $(MAKE) check

compile:
	$(EMACS) -Q --batch -L . -f batch-byte-compile $(ELFILES)

test:
	$(EMACS) -Q --batch -L . -L test \
		-l t3-code-core-test -l t3-code-thread-test -l t3-code-dashboard-test -l t3-code-integration-test \
		-f ert-run-tests-batch-and-exit

clean:
	rm -f *.elc test/*.elc
