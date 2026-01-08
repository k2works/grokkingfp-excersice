{ packages ? import <nixpkgs> {} }:
let
  baseShell = import ../../shells/shell.nix { inherit packages; };
in
packages.mkShell {
  inherit (baseShell) pure;
  buildInputs = baseShell.buildInputs ++ (with packages; [
    ruby_3_3
    bundler
    solargraph
  ]);
  shellHook = ''
    ${baseShell.shellHook}
    echo "Ruby development environment activated"
    echo "  - Ruby: $(ruby --version)"
    echo "  - Bundler: $(bundler --version)"
  '';
}
