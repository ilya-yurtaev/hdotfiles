defaul: runtest

build: clean
	stack build

install: clean
	stack install

clean:
	stack clean

runtest:
	stack test --fast --file-watch

deeptest:
	stack test --coverage --file-watch --fast --haddock
