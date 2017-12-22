
.PHONY: all scripts

all:
	npm install
	tsc

scripts:
	cd compiler; echo compile_all. | swipl compiler.pl