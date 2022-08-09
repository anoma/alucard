all:
	make clean || true
	ros run --load "alu.asd" --eval "(progn (ql:quickload :alu) (uiop:symbol-call :verbose 'remove-global-controller) (asdf:make :alu))"

ros-sbcl:
	make clean || true
	ros run --load "alu.asd" --eval "(progn (ql:quickload :alu) (uiop:symbol-call :verbose 'remove-global-controller) (uiop:symbol-call :alu 'save-alu-and-die))"

install:
	make clean || true
	make all
	mv "./build/alu.image" '${HOME}/.local/bin/'

clean:
	rm "./build/alu.image"
