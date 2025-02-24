#!/bin/bash
TARGET="hw_data/index.json"
echo '{ "updated": "'`date`'",' > $TARGET
echo '"files": [' >> $TARGET
find hw_data/cz -iname '*.json' -printf '{"dir":"%h","name":"%f", "time":"%C+"},\n' >> $TARGET
find hw_data/en -iname '*.json' -printf '{"dir":"%h","name":"%f", "time":"%C+"},\n' >> $TARGET
echo 'null' >> $TARGET
echo ']}' >> $TARGET
