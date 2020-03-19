make: src/
	elm make src/Main.elm --optimize --output=main.js

clean:
	rm -rf main.js elm-stuff