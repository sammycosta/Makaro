main: main.hs matrix.hs readpuzzle.hs certainSolutions.hs validations.hs
	ghc main.hs matrix.hs readpuzzle.hs certainSolutions.hs validations.hs

clean:
	rm -f main *.o *.hi