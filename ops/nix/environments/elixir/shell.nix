{ packages ? import <nixpkgs> {} }:
let
  baseShell = import ../../shells/shell.nix { inherit packages; };
in
packages.mkShell {
  inherit (baseShell) pure;
  buildInputs = baseShell.buildInputs ++ (with packages; [
    erlang
    elixir
    elixir-ls
  ]);
  shellHook = ''
    ${baseShell.shellHook}
    echo "Elixir development environment activated"
    echo "  - Elixir: $(elixir --version | tail -1)"
    echo "  - Erlang: $(erl -eval 'erlang:display(erlang:system_info(otp_release)), halt().' -noshell)"
  '';
}
