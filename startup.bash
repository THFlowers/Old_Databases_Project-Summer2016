#!/usr/bin/bash

mysqld_safe 2> log.txt &
/cygdrive/c/Program\ Files/Racket/Racket.exe -f main.rkt 2> log.txt 
