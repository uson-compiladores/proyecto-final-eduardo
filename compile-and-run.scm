(import (scheme base)
	(scheme write)
	(scheme process-context)
	(virtual-machine)
	(expanders)
	(reader))




(define filename (list-ref (command-line) 1))
(define debug? (if (member "debug" (command-line)) #true #false))

(define result ((if debug? run-vm-debug run-vm)
		(expand-macros (read-file filename))))

(display "\n\nRESULT ==> ") (display result)
(newline) (newline) (newline)
