main: main.hs matrix.hs readpuzzle.hs
	ghc main.hs matrix.hs readpuzzle.hs

clean:
	rm -f main *.o *.hi