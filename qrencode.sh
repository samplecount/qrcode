#!/bin/sh

stack build && stack exec qrencode -- "$@"
