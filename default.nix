{ packages ? import nix/pkgs.nix { inherit compiler; }, compiler ? "ghc8104" }:

packages.hp.callCabal2nix "fts" ./. {}
