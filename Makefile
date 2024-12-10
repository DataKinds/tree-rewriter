test: sample/regression/*.rosin
	$(foreach testfile, $^, echo 'running $(testfile)'; stack run -- $(testfile);)

tn:
	stack run -- sample/regression/$(n)* -p

build:
	stack build 

install:
	stack install


.PHONY: build install test