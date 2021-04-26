INTERACTIVE ?= --interactive

.PHONY: testbin test

test:
	touch /tmp/test-lsp.idr
	./build/exec/runtests lsp $(INTERACTIVE) --only $(only)

testbin:
	idris2 --build tests.ipkg

clean:
	$(RM) -r build
	@find . -type f -name 'output' -exec rm -rf {} \;
	@find . -type f -name '*.ttc' -exec rm -f {} \;
	@find . -type f -name '*.ttm' -exec rm -f {} \;
	@find . -type f -name '*.ibc' -exec rm -f {} \;
