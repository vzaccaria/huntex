all:
	stack build . && stack exec huntex-exe

watch:
	watchman app/Main.hs 'make all'
