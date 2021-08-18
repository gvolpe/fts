let
  inherit (import nix/pkgs.nix {}) pkgs hp;
  drv = hp.callCabal2nix "fts" ./. {};
in
drv.overrideAttrs (
  old: {
    nativeBuildInputs = old.nativeBuildInputs ++ [
      pkgs.freetype
      pkgs.glew
      pkgs.SDL2
    ];
  }
)
