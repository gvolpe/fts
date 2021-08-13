{ packages ? import ./pkgs.nix { inherit compiler; }, compiler ? "ghc8104" }:

let
  inherit (packages) pkgs;
in
  pkgs.mkShell {
    buildInputs = [ pkgs.nix-build-uncached ];
  }
