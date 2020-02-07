docker build -t sneumann/acoi_fearsome .

docker run --rm -it -v$PWD:/SUM2017 -e PASSWORD=E9zAcJfckJ -p 8787:8787  sneumann/acoi_fearsome

