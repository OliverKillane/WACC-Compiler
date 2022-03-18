# Makefile used by labTS for using cargo.

RUSTFLAGS="-C link-args=-Wl,-zstack-size=104857600"

all: debug
debug:
	cargo build && mv target/debug/compile compile
release:
	cargo build --release && mv target/release/compile compile

# clean up all of the compiled files
clean:
	rm -f compile && cargo clean

.PHONY: all clean
