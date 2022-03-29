(in-package :alu-test)

(alu:defcircuit arg-test ((public  root (bytes 64))
                          (private sig  int)
                          (private utx nested)
                          (output nested))
  3)
