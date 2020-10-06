{ nixpkgs ? (builtins.fetchTarball
  "https://github.com/NixOS/nixpkgs/archive/20.03.tar.gz") }:
import nixpkgs {

  overlays = [
    (_: pkgs: {
      haskellPackages = pkgs.haskell.packages.ghc865.override {
        overrides = self: super:
          with pkgs.haskell.lib; rec {
            ilpsched = self.callCabal2nix "ilpsched" ./. { };
          };
      };
    })
  ];
}
