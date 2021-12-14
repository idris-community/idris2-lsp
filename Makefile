include config.mk

# Idris 2 executable we're building
NAME = idris2-lsp
TARGETDIR = ${CURDIR}/build/exec
TARGET = ${TARGETDIR}/${NAME}

MAJOR=0
MINOR=1
PATCH=0

GIT_SHA1=
ifeq ($(shell git status >/dev/null 2>&1; echo $$?), 0)
	# inside a git repo
	ifneq ($(shell git describe --exact-match --tags >/dev/null 2>&1; echo $$?), 0)
		# not tagged as a released version, so add sha1 of this build in between releases
		GIT_SHA1 := $(shell git rev-parse --short=9 HEAD)
	endif
endif
VERSION_TAG ?= $(GIT_SHA1)

.PHONY: build FORCE

build: src/Server/Generated.idr
	$(IDRIS2) --build lsp.ipkg

clean:
	$(IDRIS2) --clean lsp.ipkg
	$(RM) -r build

repl: src/Server/Generated.idr
	rlwrap $(IDRIS2) --repl lsp.ipkg

testbin:
	@${MAKE} -C tests testbin

# usage: `make test only=messages001`
test-only:
	${MAKE} -C tests only=$(only)

test: build testbin test-only

install-only:
	mkdir -p ${PREFIX}/bin/
	install ${TARGET} ${PREFIX}/bin
ifeq ($(OS), windows)
	-install ${TARGET}.cmd ${PREFIX}/bin
endif
	mkdir -p ${PREFIX}/bin/${NAME}_app
	install ${TARGETDIR}/${NAME}_app/* ${PREFIX}/bin/${NAME}_app

install: build install-only

# We use FORCE to always rebuild IdrisPath so that the git SHA1 info is always up to date
src/Server/Generated.idr: FORCE
	echo "-- @""generated" > src/Server/Generated.idr
	echo 'module Server.Generated' >> src/Server/Generated.idr
	echo 'export idrisLSPVersion : ((Nat,Nat,Nat), String); idrisLSPVersion = ((${MAJOR},${MINOR},${PATCH}), "${VERSION_TAG}")' >> src/Server/Generated.idr

FORCE:
