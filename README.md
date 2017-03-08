# blob
An agar.io bot that tries to compete with humans

## Goal

Input: A sequence of the last N frames with:
- positions of other blobs in the neighborhood and their types:
  - `food`
  - `player`
  - `mine`

Output:
- direction
- an optional action:
  - `split`
  - `feed`

Score:
 - weighted
    - `size`
    - `time lived`
 - `score` provided by the server

## Method

A form of evolutional programming is going to be used.

The programs are going to be represented either as Lisp code (s-expressions)
or as an encoding of it (some form of tree encoding).

## Plan

  - [ ] research existing solutions
	- [Existing agar.io bots](existing-solutions.md#existing-agario-bots)
	- [Websocket-based agar.io clients](existing-solutions.md#websocket-based-agario-clients)
	- [Open-source agar.io servers](existing-solutions.md#open-source-agario-servers)
  - [ ] give a presentation
  - [ ] play on external server
  - [ ] play on internal server
  - [ ] compare results from internal and external training
	- quality
	- speed
  - [ ] compare the results
	- `ai` vs `pro` vs `amateur`
  - [ ] give a presentation again
  - [ ] add teamplay
  - [ ] profit
