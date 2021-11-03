include config.mk

# Idris 2 executable we're building
NAME = idris2-lsp
TARGETDIR = ${CURDIR}/build/exec
TARGET = ${TARGETDIR}/${NAME}

.PHONY: build

build:
	$(IDRIS2) --build lsp.ipkg

clean:
	$(IDRIS2) --clean lsp.ipkg
	$(RM) -r build

repl:
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

