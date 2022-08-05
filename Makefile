all:
	make clean
	ros run --load "alu.asd" --eval "(progn (ql:quickload :alu) (asdf:make :alu))"

install:
	make clean
	ros run --load "alu.asd" --eval "(progn (ql:quickload :alu) (asdf:make :alu))"
	mv "./build/alu.image" '${HOME}/.local/bin/'

clean:
	rm "./build/alu.image"
