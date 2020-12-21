build: | game clean

game: src/*.hs
	cd src; \
	ghc --make -o ../Game ./Main.hs

.PHONY: clean
clean:
	-rm src/*.hi
	-rm src/*.o
