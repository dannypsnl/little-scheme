.PHONY: build test install
build:
	@meson target
	@ninja -C target
test: build
	@ninja -C target test
install: build
	@ninja -C target install
