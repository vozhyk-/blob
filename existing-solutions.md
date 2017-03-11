# Existing solution evaluation

## Existing agar.io bots

- https://github.com/heyitsleo/agar.io-bot:
  Doesn't work - only the bot interface is shown; the game portion of the screen is grey. Tested on Chrome 56 on Windows 10.
- https://github.com/Apostolique/Agar.io-bot:
  Doesn't work - tries connecting to a server and fails every time. Tested on TamperMonkey on Chrome 56 on Windows 10.

## Websocket-based agar.io clients

- https://github.com/ibash/agar:
  Doesn't build - errors when compiling C++ code when running `npm install`
- https://github.com/Raeon/pygar
  Doesn't run - Hangs when importing libraries (only killed by pressing `^\` 3 times - once for every package being imported). Run with Python 3.4.5 after installing the packages mentioned in README in a virtualenv.

## Unobfuscated in-browser agar.io clients

- https://github.com/CigarProject/Cigar: Works with `MultiOgar-Edited`. Doesn't work with agar.io - tries connecting and fails every time.

## Open-source agar.io servers

They might have a well-defined protocol, so writing a client for them should be much easier.

- https://github.com/huytd/agar.io-clone: Works. Flawless setup in docker container. `pygar` fails to connect. Communication is [documented](https://github.com/huytd/agar.io-clone/wiki/Game-Architecture#client-server-communication). Only a few configuration values available in `config.json`.
- https://github.com/OgarProject/Ogar: Requires changes to start. Configurable. Does not work (does not listen).
- https://github.com/Megabyte918/MultiOgar-Edited: Works. Requires an [external client](https://github.com/Megabyte918/MultiOgar-Edited/wiki/Clients).
