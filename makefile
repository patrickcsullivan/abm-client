make: ./src/
	elm make ./src/Main.elm --output=./www/main.js

run: make
	http-server ./www -p 8080

clean:
	rm -rf ./www/main.js ./elm-stuff