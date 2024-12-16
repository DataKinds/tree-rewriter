ROSIN?=stack run --

test: sample/regression/*.rosin
	$(foreach testfile, $^, echo ''; echo 'running $(testfile)\n-----------------------------'; $(ROSIN) $(testfile);)

tn:
	$(ROSIN) sample/regression/$(n)* -p

build:
	stack build 

install:
	stack install


.PHONY: build install test