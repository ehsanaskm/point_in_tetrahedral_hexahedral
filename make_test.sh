#!/bin/sh

gfortran crosstest.f95 -o test.exe

./test.exe
