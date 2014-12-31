Spoj : Rent.hs
	sed '1d;2d' Rent.hs             >Spoj.hs
	cat Main.hs                    >>Spoj.hs
	ghc --make -O2 Spoj.hs

clean:
	rm *.hi
	rm *.o
	rm Spoj
	rm *.dat

spec:
	runghc Specs.hs

test:
	runghc Tests.hs


large:  
	runghc GenerateTestFile M >largedata.dat	
		
perf: Spoj 
	ghc --make -O2 -fforce-recomp Spoj.hs
	(time ./Spoj <largedata.dat ) 2> results.md 

prof: Spoj
	ghc --make -O2 -rtsopts -prof -auto-all -fforce-recomp Spoj.hs
	./Spoj +RTS -sstderr -p <largedata.dat
