main: main.hs matrix.hs readpuzzle.hs certainSolutions.hs validations.hs positionUtils.hs solveRegion.hs solve.hs
	ghc main.hs matrix.hs readpuzzle.hs certainSolutions.hs validations.hs positionUtils.hs solveRegion.hs solve.hs

clean:
	rm -f main *.o *.hi