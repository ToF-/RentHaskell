Spoj : Rent.hs
	sed '1d;2d' Rent.hs             >Spoj.hs
	echo "main = interact process" >>Spoj.hs 
	ghc --make Spoj.hs

clean:
	rm *.hi
	rm *.o
	rm Spoj
	rm *.txt
