{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    ghc
    ghcid
    stack
    emacs-gtk
    direnv
    haskell-language-server
    haskellPackages.cabal-install
    haskellPackages.parsec
    haskellPackages.filepath
  ];
  shellHook = ''
    cabal repl
  '';
}
