defaul: test


build: clean
	stack build --haddock


install: clean
	stack install


clean:
	stack clean


test:
	stack test --fast


watch:
	stack test --fast --file-watch


deeptest:
	stack test --fast --coverage --haddock


format:
	brittany --write-mode=inplace **/*.hs
