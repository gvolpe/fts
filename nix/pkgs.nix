{ compiler ? "ghc8104" }:

let
  pkgs = import (
    builtins.fetchTarball {
      name = "nixos-unstable-2021-08-01";
      url = "https://github.com/NixOS/nixpkgs/archive/8ecc61c91a5.tar.gz";
      sha256 = "0vhajylsmipjkm5v44n2h0pglcmpvk4mkyvxp7qfvkjdxw21dyml";
    }
  ) {};

  hp = pkgs.haskell.packages.${compiler}.override {
    overrides = new: old: rec {
      monomer =
        pkgs.haskell.lib.dontCheck (
          new.callCabal2nix "monomer" (
            builtins.fetchGit {
              url = "https://github.com/fjvallarino/monomer.git";
              rev = "c1903ed153243aadb12a838bfc1238b4e25a6bcf";
            }
          ) { GLEW = pkgs.glew; }
        );

      nanovg =
        pkgs.haskell.lib.dontCheck (
          new.callCabal2nixWithOptions "nanovg" (
            builtins.fetchGit {
              url = "https://github.com/cocreature/nanovg-hs.git";
              rev = "cc8dfa0dc18a0792786c973b4e9a232fa7d3ecfd";
            }
          ) "-fexamples -fstb_truetype" {
            GLEW = null;
            inherit (pkgs) glew;
            inherit (pkgs) libGL;
            inherit (pkgs) libGLU;
            inherit (pkgs.xorg) libX11;
          }
        );

      postgresql-resilient =
        pkgs.haskell.lib.dontCheck (
          new.callCabal2nix "postgresql-resilient" (
            builtins.fetchGit {
              url = "https://github.com/gvolpe/postgresql-resilient.git";
              rev = "5544240a7e7e7077f7828f8941407254f3de8b62";
              ref = "refs/heads/nix/remove-cabal-bounds";
            }
          ) {}
        );
    };
  };
in
{
  pkgs = pkgs;
  hp = hp;
}
