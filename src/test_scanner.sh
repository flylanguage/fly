#!/bin/bash
dune build test_scanner.exe
dune exec test_scanner.exe -- $1