	all:
	make clean || true
	ros run --load "alu.asd" --eval "(progn (load \"alu.asd\") (make-system))"

install:
	make clean || true
	make all
	mv "./build/alu.image" '${HOME}/.local/bin/'

clean:
	rm "./build/alu.image"
