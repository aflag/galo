Test: *.hs
	ghc --make Test.hs
Galo: *.hs
	ghc --make Galo.hs
clean:
	rm -f *.o *.hi Galo Test
