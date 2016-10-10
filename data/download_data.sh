#!/bin/bash

wget https://cityofsyracuse.github.io/RoadsChallenge/data/roads/RoadRatings{2000..2015}.csv
wget https://cityofsyracuse.github.io/RoadsChallenge/data/potholes.csv
wget https://cityofsyracuse.github.io/RoadsChallenge/data/street_shapefile.zip

unzip street_shapefile.zip

echo "Done."
