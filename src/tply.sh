#!/bin/bash

../lib/tply/plex -o lexer.l lexer.inc
../lib/tply/pyacc -v parser.y
