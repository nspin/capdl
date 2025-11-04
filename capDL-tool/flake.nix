{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-25.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        ghcVersion = "9103";
        hls = pkgs.haskell-language-server.override {
          supportedGhcVersions = [ ghcVersion ];
        };
      in rec {
        devShells.default = pkgs.mkShell {
          packages = [
            hls
          ] ++ (with pkgs; [
            stack
          ]);
        };
      }
    );
}
