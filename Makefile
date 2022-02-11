# Makefile used by labTS for using cargo.

all:
	cargo build --release && mv target/release/compile compile

# clean up all of the compiled files
clean:
	rm -f compile && cargo clean

.PHONY: all clean
