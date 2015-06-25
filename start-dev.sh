#!/bin/sh

exec erl -sname esess \
    -config env/esess \
	-pa ebin/ deps/*/ebin \
    -boot start_sasl \
    -s reloader \
    -s esess
