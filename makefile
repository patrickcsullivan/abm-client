make: ./ui/src/
	( cd ./ui && elm make ./src/Main.elm --output=../www/main.js )

run: make
	http-server ./www -p 8080

clean:
	rm -rf ./www/main.js ./ui/elm-stuff