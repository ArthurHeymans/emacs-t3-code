EMACS ?= emacs
ELFILES := t3-code-core.el t3-code-markdown.el t3-code-shell.el t3-code-render.el t3-code-thread.el t3-code-dashboard.el t3-code.el
TESTFILES := test/t3-code-core-test.el test/t3-code-shell-test.el test/t3-code-thread-test.el test/t3-code-dashboard-test.el test/t3-code-integration-test.el
T3CODE_DIR ?= ../t3code
# Optional packages exercised by the tests when installed (markdown-mode).
EXTRA_LOAD_PATH ?= $(wildcard $(HOME)/.emacs.d/elpa/markdown-mode-*)

.PHONY: check check-t3code test compile clean

check: compile test

# Opt-in compatibility gate for the adjacent T3 source checkout.
check-t3code:
	cd $(T3CODE_DIR)/apps/server && ./node_modules/.bin/vp test run src/cli/client.test.ts
	T3CODE_DIR=$(abspath $(T3CODE_DIR)) $(MAKE) check

compile:
	$(EMACS) -Q --batch -L . -f batch-byte-compile $(ELFILES)

test:
	$(EMACS) -Q --batch -L . -L test $(addprefix -L ,$(EXTRA_LOAD_PATH)) \
		-l t3-code-core-test -l t3-code-shell-test -l t3-code-thread-test -l t3-code-dashboard-test -l t3-code-integration-test \
		-f ert-run-tests-batch-and-exit

clean:
	rm -f *.elc test/*.elc
