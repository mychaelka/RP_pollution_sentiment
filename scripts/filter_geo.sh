awk -F "|"  '$5=="1"  {print}' ../data/tweets/all/07-2015.csv > ../data/tweets/all/07-2015_geo.csv
