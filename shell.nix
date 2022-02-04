{ pkgs ? import (import ./nix/sources.nix).nixpkgs {} }:
pkgs.mkShell {
    buildInputs = with pkgs; [
        # libs
        lzma
        zlib

        # required to build in a pure nix shell
        git
        cacert # git SSL
        pkg-config # required by libsystemd-journal

        # build haskell
        haskell.compiler.ghc8104
        haskellPackages.cabal-install

        # devtools
        haskell-language-server
        hlint
        ormolu
        hpack
        python38
        python38Packages.pip
        python38Packages.spacy
        python38Packages.spacy_models.en_core_web_sm
    ];
}
