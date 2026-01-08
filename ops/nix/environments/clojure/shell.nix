{ packages ? import <nixpkgs> {} }:
let
  baseShell = import ../../shells/shell.nix { inherit packages; };
in
packages.mkShell {
  inherit (baseShell) pure;
  buildInputs = baseShell.buildInputs ++ (with packages; [
    jdk21
    clojure
    leiningen
    clojure-lsp
  ]);
  shellHook = ''
    ${baseShell.shellHook}
    echo "Clojure development environment activated"
    echo "  - JDK: $(java -version 2>&1 | head -1)"
    echo "  - Clojure: $(clojure --version 2>&1)"
  '';
}
