#!/bin/sh

echo "Wait for other services start"
sleep 5
# rm -Rf deps
# make deps
# make compile
make tests
