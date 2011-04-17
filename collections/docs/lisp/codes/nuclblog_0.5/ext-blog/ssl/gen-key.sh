#!/bin/sh
openssl genrsa -out key.pem 1024      
openssl req -config openssl.config -new -key key.pem -out request.pem
openssl req -x509 -days 365 -key key.pem -in request.pem -out certificate.pem


