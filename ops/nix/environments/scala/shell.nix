{ packages ? import <nixpkgs> {} }:
let
  baseShell = import ../../shells/shell.nix { inherit packages; };
in
packages.mkShell {
  inherit (baseShell) pure;
  buildInputs = baseShell.buildInputs ++ (with packages; [
    jdk21
    sbt
    scala_3
    metals
  ]);
  shellHook = ''
    ${baseShell.shellHook}
    echo "Scala development environment activated"
    echo "  - JDK: $(java -version 2>&1 | head -1)"
    echo "  - sbt: $(sbt --version 2>&1 | tail -1)"
  '';
}
