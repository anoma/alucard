(in-package :alu-test)

(alu:defcircuit arg-test ((public  root (bytes 64))
                          (private sig  int)
                          (private utx nested)
                          (output nested))
  3)


(alu:deftype utxo ()
  (owner  (bytes 128))
  (amount (int   64))
  (nonce  (int   64)))

(deftype point ()
  (x int)
  (y int))

(deftype nested ()
  (plane point)
  (time  point))
