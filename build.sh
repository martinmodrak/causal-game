#!/bin/bash
elm make src/Main.elm --output=dist/elm.js --optimize
cp index.html dist
cp style.css dist