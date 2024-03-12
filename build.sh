#!/bin/sh

set -xe

zig build run | qbe > temp.S && zig cc temp.S
rm temp.S
