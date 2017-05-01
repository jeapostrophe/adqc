all:
	raco make *.rkt
	raco test .
	ag xxx
	rktwc
