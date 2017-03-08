# Existing solution evaluation

## Existing agar.io bots

- https://github.com/heyitsleo/agar.io-bot:
  Doesn't work - only the bot interface is shown; the game portion of the screen is grey. Tested on Chrome 56 on Windows 10.
- https://github.com/Apostolique/Agar.io-bot:
  Doesn't work - tries connecting to a server and fails every time. Tested on TamperMonkey on Chrome 56 on Windows 10.

## websocket-based agar.io clients

- https://github.com/ibash/agar:
  Doesn't build - errors when compiling C++ code when running `npm install`
- https://github.com/Raeon/pygar
  Doesn't run - Hangs when importing libraries (only killed by pressing `^\` 3 times - once for every package being imported). Run with Python 3.4.5 after installing the packages mentioned in README in a virtualenv.
