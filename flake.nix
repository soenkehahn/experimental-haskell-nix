{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.05";
    cradle = {
      url = "github:garnix-io/cradle";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
      };
    };
  };
  outputs = inputs@{ self, nixpkgs, flake-utils, ... }:
    let configure = import ./lib.nix { inherit inputs; };
    in
    nixpkgs.lib.recursiveUpdate
      ({ inherit configure; })
      (
        flake-utils.lib.eachDefaultSystem (system:
          let pkgs = import nixpkgs { inherit system; };
          in import ./tests { inherit self inputs system pkgs configure; })
      );
}
