#!/bin/bash
TARGET="hw_data/index.json"
echo '{ "updated": "'`date`'",' > $TARGET
echo '"files": [' >> $TARGET
find hw_data -iname homework* -printf '"%f",\n' >> $TARGET
echo 'null' >> $TARGET
echo ']}' >> $TARGET