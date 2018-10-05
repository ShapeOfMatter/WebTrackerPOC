# WebTrackerPOC

## Goals

- User associates a (potentially new) username with a browser-session (cookie). ✓
- Requests to a particular address have the http-referer logged with the corrisponding username. ✓

## Next Steps

- **Confirm that a webpage can't fire spoofed "Referer" (and/or "Origin") headers from an unconsenting browser.**
- Decide if [Persistent](https://www.yesodweb.com/book/persistent) (a Template-Haskell based Domain Specific Language for talking to databases) would be more appropreate than [Hasql](https://github.com/nikita-volkov/hasql).
- Decide if [Yesod](https://www.yesodweb.com/) (a heavier, more complete Haskell web framework) would be more approoreate than [Scotty](https://hackage.haskell.org/package/scotty-0.11.2)
- Generally refactor stuff to have a flatter, more idiomatic layout.
- Consider building our own Monad stacks, or use existing stacks better for validation and database interaction.
- Move type changes to the perifiery, and make them automatic (using type classes) where possible. Have more of our own types for business atoms.
- Maybe have fewer in-line type declarations, and let function declarations be more vague (via typeclases).
- Use DB transactions.

## Installation

A brand new ubuntu-style linux box is assumed.

Some of the installation steps are very ram-hungry. On smaller servers I suggest using the instructions [here](https://www.digitalocean.com/community/tutorials/how-to-add-swap-space-on-ubuntu-16-04) to give yourself extra swap space (try 4G).

This is all taken from a tutorial [here](https://github.com/Gabriel439/haskell-nix).  
Install Nix blindly, or figure out the proper secure way of doing it.
```bash
curl https://nixos.org/nix/install | sh
```
There will be a second step, which the output of the above should promt you for. something like:
```bash
. /home/ubuntu/.nix-profile/etc/profile.d/nix.sh
```
Next use nix to set up some Haskell specific utilities.
```bash
nix-env --install cabal2nix
nix-env --install nix-prefetch-git
nix-env --install cabal-install
```
Download and build the project
```bash
git clone https://github.com/ShapeOfMatter/WebTrackerPOC.git
cd WebTrackerPOC/
cabal2nix . > default.nix
nix-build unpinned.nix
```
You'll need a config file. For now it has to be named "defaults.config", and it should go right in the WebTrackerPOC directory.
```
[DatabaseConnectionPool]
host = "example.com"
port = 5432
user = "example"
password = "xxx"
database = "example"
maxconnections = 5
maxidleseconds = 60
[SiteSettings]
baseurl = http://example.com
port = 80
```
Finally, you can set the server running just by calling:
```bash
result/bin/poc_scotty_server
```
But remember to disown the process from your shell before loggin out.


