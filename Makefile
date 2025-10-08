.PHONY: default build install uninstall test clean fmt
.IGNORE: fmt

OPAM ?= opam
OPAM_EXEC ?= $(OPAM) exec --
DUNE ?= dune
SERVE ?= python -m http.server --dir ..

default: build

fmt:
	$(OPAM_EXEC) $(DUNE) build @fmt
	$(OPAM_EXEC) $(DUNE) promote
	$(OPAM_EXEC) $(DUNE) format-dune-file dune-project > .dune-project-formatted
	mv .dune-project-formatted dune-project

build: fmt
	$(OPAM_EXEC) $(DUNE) build
	install -Dm644 _build/default/bin/main.bc.js static/main.bc.js

install:
	$(OPAM_EXEC) $(DUNE) install

uninstall:
	$(OPAM_EXEC) $(DUNE) uninstall

clean:
	$(OPAM_EXEC) $(DUNE) clean
	git clean -dfXq

test: fmt build
	$(OPAM_EXEC) $(DUNE) runtest

testf: fmt
	$(OPAM_EXEC) $(DUNE) runtest -f

serve: test
	$(SERVE)

run: test
	(sleep 1 && firefox http://127.0.0.1:8000/rng/) &
	$(SERVE)

