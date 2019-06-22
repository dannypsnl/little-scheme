.PHONY: build test install
build: target
	@cd target && cmake -G Ninja ..
	@ninja -C target
test: build
	@./target/src/tests
install: build
	@ninja -C target install

target:
	@mkdir -p target
