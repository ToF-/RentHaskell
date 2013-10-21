spec: Rent.hs Specs.hs
	ghc --make Specs.hs -o Specs
	./Specs
spike: Spike.hs
	ghc --make Spike.hs -o Spike
	./Spike
