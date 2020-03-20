# elm-2048

### [Play it here](http://wilspi.github.io/elm-2048/)  


This is an implementation of [2048](http://gabrielecirulli.github.io/2048/) game in Elm


### Development
* Install `nix`  
  Follow steps [here](https://gist.github.com/wilspi/aad81f832d030d80fca91dfa264a1f8a), if not done already
* Run `nix` env:
  ```
  nix-shell --pure shell.nix
  ```
* Development:
  ```
  elm --help
  elm-format src/Main.elm --yes
  elm make src/Main.elm
  elm reactor
  ```
