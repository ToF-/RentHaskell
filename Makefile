Spoj : Rent.hs
	sed '1d;2d' Rent.hs             >Spoj.hs
	echo "main = interact process" >>Spoj.hs 
	ghc --make -O2 Spoj.hs

clean:
	rm *.hi
	rm *.o
	rm Spoj
	rm *.txt

large:  
	runghc GenerateTestFile M >largedata.txt	
		
perf: Spoj 
	ghc --make -O2 -fforce-recomp Spoj.hs
	(time ./Spoj <largedata.txt ) 2> results.md 

prof: Spoj
	ghc --make -O2 -rtsopts -prof -auto-all -fforce-recomp Spoj.hs
	./Spoj +RTS -sstderr -p <largedata.txt
