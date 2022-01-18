{ 
    pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/nixos-21.11.tar.gz") {}
}:
let
  inherit (pkgs) haskell haskellPackages mkShell;

  haskell-deps = pkgs: with pkgs; [
      base
      matrix
      string-interpolate
      brick
  ];

  ghc = haskellPackages.ghcWithPackages haskell-deps;

in
pkgs.mkShell {
    buildInputs = [
        ghc
        haskellPackages.hlint
        haskellPackages.stylish-haskell
        haskellPackages.haskell-language-server
    ];
}