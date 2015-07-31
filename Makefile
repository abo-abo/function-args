emacs ?= emacs

PWD = $(shell pwd)

all: check

compile:
	$(emacs) -batch -l targets/compile.el

check:
	emacs -q -eval "(progn (add-to-list 'load-path \"$(HELM_DIR)\")(load \"$(PWD)/function-args.el\"))" \
	-f fa-config-default $(TESTFILE)
clean:
	rm -f *.elc
