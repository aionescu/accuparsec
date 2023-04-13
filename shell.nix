with import
  (fetchTarball {
    name = "nixos-22.11-2023-03-06";
    url = "https://github.com/NixOS/nixpkgs/archive/7edcdf7b169c33cd3eef9aba50521ce93ee666b8.tar.gz";
    sha256 = "05rpnsnkwibj36vcmxd55ms2brl3clbi5gh5cnks6qaw2x6mdsag";
  })
    {};
mkShell {
  packages =
    [
      haskell.compiler.ghc943
      cabal-install
      (python310.withPackages (ps: [
        ps.numpy ps.matplotlib
      ]))
    ];
}
