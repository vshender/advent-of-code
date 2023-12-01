SRC := $(wildcard *.ml)
BIN := $(patsubst %.ml,%.exe,$(SRC))

.PHONY: solve clean

solve: $(BIN)
	./$(BIN)

%.exe: %.ml
	ocamlopt -o $@ $<

clean:
	rm -f *~ \#*
	rm -f $(BIN) *.o *.cm[ix]
