#!/bin/bash
elm make src/Main.elm --output=docs/elm-big.js --optimize
minify -o docs/elm.js docs/elm-big.js
rm docs/elm-big.js
cp index.html docs
cp style.css docs

