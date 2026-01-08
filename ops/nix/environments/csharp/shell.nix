{ packages ? import <nixpkgs> {} }:
let
  baseShell = import ../../shells/shell.nix { inherit packages; };
in
packages.mkShell {
  inherit (baseShell) pure;
  buildInputs = baseShell.buildInputs ++ (with packages; [
    dotnet-sdk_8
    omnisharp-roslyn
  ]);
  shellHook = ''
    ${baseShell.shellHook}
    export DOTNET_CLI_TELEMETRY_OPTOUT=1
    echo "C# development environment activated"
    echo "  - .NET SDK: $(dotnet --version)"
  '';
}
