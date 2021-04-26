testbin:
	@${MAKE} -C tests testbin

# usage: `make test only=messages001`
test-only:
	${MAKE} -C tests only=$(only)

test: testbin test-only
