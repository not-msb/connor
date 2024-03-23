#!/bin/sh

set -xe

zig build run | qbe > temp.S
zig build-exe temp.S lib.S -femit-bin=main
rm temp.S
