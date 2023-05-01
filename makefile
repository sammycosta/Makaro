main: main.hs matrix.hs readpuzzle.hs certainSolutions.hs
	ghc main.hs matrix.hs readpuzzle.hs certainSolutions.hs

clean:
	rm -f main *.o *.hi