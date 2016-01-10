# elm carcassonne

The Carcassonne board game in Elm-Lang.


Todo / Further Ideas:

- model the idea of a Player and turns, start tracking turns through the game
- allow game tiles to be rotated round before being placed

a turn has stages:
- place the tile
- (optionally) place a person
- check if the tile placement means any people are picked up, and update points accordingly
- end

## Running Locally

Clone, `npm install` and then:

- `gulp test` to run tests with Elm-Test
- `gulp serve` to run the app on a live server locally
- `gulp build` to compile the Elm app
- `gulp start` to watch and recompile Elm when files change
