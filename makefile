main: main.hs matrix.hs readpuzzle.hs certainSolutions.hs validations.hs positionUtils.hs 
	ghc main.hs matrix.hs readpuzzle.hs certainSolutions.hs validations.hs positionUtils.hs 

clean:
	rm -f main *.o *.hi