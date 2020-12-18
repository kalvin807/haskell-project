Game: *.hs
	ghc -o Game Main.hs

.PHONY: clean
clean:
	-rm *.hi
	-rm *.o
	-rm Game
