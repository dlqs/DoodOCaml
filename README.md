[DoodOcaml](https://dlqs.github.io/DoodOCaml/)
=============
DoodOcaml is an HTML 5 canvas web-browser implementation of the Doodle Jump game written in Ocaml.

<img src="https://github.com/dlqs/DoodOcaml/raw/master/screenshots/screenshot1.png" height="300">

[Try it out!](https://dlqs.github.io/DoodOCaml/)

## Key Features
* Web-based playable implementation with graphics (Ocaml transpiled to javascript).
* Procedural level generator that randomly generates playable levels.

## Description
Follows the original repo in terms of file structure but almost all the code is rewritten. Everything has been written from scratch (not sure if that's a good thing...), from the loading of sprites to particle collision detection.

## Building the Project
1. Install Dune, then js_of_ocaml.

2. Run `eval opam config env`

3. Run `dune build ./main.bc.js`  
 Dune will warn you about `main.bc.js` already existing. Delete the one in the top level (it's the one for github pages) and re-run the command. When you're done simply re-copy the file to the top level so that gihub pages can see it.

4. Open `index.html` to play!

