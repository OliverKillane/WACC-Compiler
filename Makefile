# Makefile used by labTS for using cargo.

all:
	cargo build && mv target/debug/compile compile

# clean up all of the compiled files
clean:
	rm -f compile && cargo clean

.PHONY: all clean
