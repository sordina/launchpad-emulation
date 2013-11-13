all:
	(sleep 2; chromereload 10000) &
	runhaskell Main.hs

devel:
	commando -q -p cat -j | uniqhash | grep --line-buffered hs | conscript make
