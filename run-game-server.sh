#!/bin/sh

# This script will run your game server locally. For help do
# [$ ./run-game-server.sh --help]
#
# It will also build the executables for you, if they are not
# present.

set -euo pipefail
set -x

SERVER_EXE="_build/default/server/bin/main.exe"
CLIENT_JS="_build/default/client/bin/main.bc.js"

if test -f "$SERVER_EXE"; then
  if test -f "$CLIENT_JS"; then
    $SERVER_EXE -js-file "$CLIENT_JS" $@
    exit 0;
  fi
fi

dune build
$SERVER_EXE -js-file "$CLIENT_JS" $@

