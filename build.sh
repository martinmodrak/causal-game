#!/bin/bash
elm make src/Main.elm --output=docs/elm.js --optimize
cp index.html docs
cp style.css docs