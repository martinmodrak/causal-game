#!/bin/bash
./index_hw.sh
echo "HWs indexed"
elm-live src/HWView.elm -- --output=hwview.js
