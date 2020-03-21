make: src/
	elm make src/Main.elm --output=main.js

clean:
	rm -rf main.js elm-stuff