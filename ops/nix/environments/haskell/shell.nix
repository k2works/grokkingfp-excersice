{ packages ? import <nixpkgs> {} }:
let
  baseShell = import ../../shells/shell.nix { inherit packages; };
in
packages.mkShell {
  inherit (baseShell) pure;
  buildInputs = baseShell.buildInputs ++ (with packages; [
    ghc
    stack
    cabal-install
    haskell-language-server
  ]);
  shellHook = ''
    ${baseShell.shellHook}
    echo "Haskell development environment activated"
    echo "  - GHC: $(ghc --version)"
    echo "  - Stack: $(stack --version)"
  '';
}
