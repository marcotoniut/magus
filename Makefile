install:
	stack build --copy-bins

run:
	stack exec magus

build:
	stack build

buildr:
	stack build && stack exec magus

watch:
	stack build --file-watch

watchr:
	find src/ -type f | entr -r -c stack build --exec magus

deploy:
	git push heroku master
