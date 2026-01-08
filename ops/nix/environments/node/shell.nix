{ packages ? import <nixpkgs> {} }:
let
  baseShell = import ../../shells/shell.nix { inherit packages; };
in
packages.mkShell {
  inherit (baseShell) pure;
  buildInputs = baseShell.buildInputs ++ (with packages; [
    nodejs_22
    nodePackages.npm
    nodePackages.yarn
    # AI tools
    nodePackages."@anthropic-ai/claude-code"
    # Note: gemini-cli and copilot CLI might not be in standard nixpkgs under these names
    # or might need specific versions. Using what's typically available.
  ]);
  shellHook = ''
    ${baseShell.shellHook}
    echo "Node.js development environment activated"
  '';
}
