.PHONY: all
all:
	dune build

.PHONY: test
test:
	dune runtest

.PHONY: clean
clean:
	dune clean
