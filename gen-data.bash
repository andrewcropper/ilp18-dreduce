trials=20

rm -rf 'data'
mkdir 'data'
yap -g "main,halt" -l gen-trains.pl > "data/trains-tmp.pl"
< "data/trains-tmp.pl" tail -n +3 | tail -r | tail -n +5 | tail -r > "data/trains.pl"
rm "data/trains-tmp.pl"
yap -g "main($trials),halt" -l gen-data.pl