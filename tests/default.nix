{ pkgs, inputs, system, ... }:
let
  lib = pkgs.lib;
  haskellPackages = pkgs.haskellPackages.override
    {
      overrides = final: prev: {
        cradle = inputs.cradle.lib.${system}.mkCradle final;
      };
    };
  runTestsPackage =
    let
      src = lib.fileset.toSource {
        root = ./.;
        fileset = lib.fileset.fileFilter
          (file: file.name == "package.yaml")
          ./.;
      };
    in
    haskellPackages.callCabal2nix "runTests" src { };
  ghc = haskellPackages.ghc.withPackages (p: runTestsPackage.buildInputs);
in
{
  devShells.default = pkgs.mkShell {
    name = "simple-hpack";
    buildInputs = [
      ghc
      haskellPackages.cabal-install
      haskellPackages.hpack
      (pkgs.haskell-language-server.override { dynamic = true; })
    ];
  };
  apps."runTests" = {
    type = "app";
    program = pkgs.lib.getExe
      (pkgs.writeShellApplication {
        name = "runTests";
        runtimeInputs = [ ghc ];
        text = ''
          #!${pkgs.bash}/bin/bash
          set -euo pipefail
          tmp=$(mktemp -d)
          trap 'rm -rf "$tmp"' EXIT
          cd "$tmp"
          cp -r ${./..}/. .
          chmod +rwX . -R
          cd tests
          runhaskell ./RunTests.hs;
        '';
      });
  };
}
