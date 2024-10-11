#!/usr/bin/env bash

set -x
yarn esbuild vendor/src/time_impl.js --minify --bundle --target=es2022 --outfile=vendor/time_impl.js --format=esm
#--log-level=silent
