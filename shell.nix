with import ./. { };

haskellPackages.shellFor {
  packages = p: [ haskellPackages.ilpsched ];
  withHoogle = true;
  buildInputs = [
    ghcid
    gnuplot
    ormolu
    haskellPackages.hlint
    cabal-install
    feh
  ];
}
