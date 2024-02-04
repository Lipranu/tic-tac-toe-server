{
  description = "Server for playing in tic-tac-toe";

  nixConfig.bash-prompt = "[tic-tac-toe-server]: ";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        haskellPackages = pkgs.haskellPackages;
        tic-tac-toe-server = haskellPackages.callCabal2nix "tic-tac-toe-server" self rec {};
      in {
        defaultPackage = tic-tac-toe-server;
        packages = { inherit tic-tac-toe-server; };
        devShell = tic-tac-toe-server.env.overrideAttrs (super: {
          nativeBuildInputs = with haskellPackages;
            super.nativeBuildInputs ++ [
              haskell-language-server
              haskell-ci
              cabal-install
              ghcid
            ];
        });
      }
    );
}
