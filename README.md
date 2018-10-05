# WebTrackerPOC

## Goals

- User associates a (potentially new) username with a browser-session (cookie). ✓
- Requests to a particular address have the http-referer logged with the corrisponding username. ✓

## Installation

A brand new ubuntu-style linux box is assued.

Some of the installation steps are very ram-hungry. On smaller servers I suggests using the instructions [here](https://www.digitalocean.com/community/tutorials/how-to-add-swap-space-on-ubuntu-16-04) to give yourself extra swap space (try 4G).

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


