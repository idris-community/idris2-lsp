INTERACTIVE ?= --interactive

.PHONY: testbin test

test: clean testbin
	touch /tmp/test-lsp.idr
	./build/exec/runtests lsp $(INTERACTIVE) --only $(only)

testbin:
	# hack so `1/1: Building lsp.messages002.Foo` doesn't appear in test output
	idris2 --check ./lsp/messages002/Foo.idr
	idris2 --build tests.ipkg

clean:
	$(RM) -r build
	@find . -type f -name 'output' -exec rm -rf {} \;
	@find . -type f -name '*.ttc' -exec rm -f {} \;
	@find . -type f -name '*.ttm' -exec rm -f {} \;
	@find . -type f -name '*.ibc' -exec rm -f {} \;
