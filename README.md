# blob
An agar.io bot that tries to compete with humans

## Goal

Input: Positions of other blobs in the neighborhood and their types:
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

Genetic algorithm will be used.

## Plan

  - [ ] research existing solutions
	- `ai`
	- `server`
	- `client`
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
