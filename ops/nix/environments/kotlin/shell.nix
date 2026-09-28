{ packages ? import <nixpkgs> {} }:
let
  baseShell = import ../../shells/shell.nix { inherit packages; };
in
packages.mkShell {
  inherit (baseShell) pure;
  buildInputs = baseShell.buildInputs ++ (with packages; [
    jdk21
    gradle
    kotlin
  ]);
  shellHook = ''
    ${baseShell.shellHook}
    echo "Kotlin development environment activated"
    echo "  - JDK: $(java -version 2>&1 | head -1)"
    echo "  - Gradle: $(gradle --version 2>&1 | grep Gradle)"
    echo "  - Kotlin: $(kotlin -version 2>&1)"
  '';
}
