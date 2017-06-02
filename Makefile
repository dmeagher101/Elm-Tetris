all: tetris

tetris:
	elm-make src/Tetris.elm --output=index.html

clean:
	rm -rf *.html elm-stuff
