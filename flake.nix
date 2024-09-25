{
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-24.05-small";
  outputs = { self, nixpkgs, flake-utils }:
    {
      configure = args:
        let
          lib = nixpkgs.lib;
          fs = lib.fileset;
          configSource = fs.toSource {
            root = args.root;
            fileset =
              let filter = file: file.name == "package.yaml";
              in fs.fileFilter filter args.root;
          };
        in
        flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
          let
            pkgs = nixpkgs.legacyPackages.${system};
            packageYaml = builtins.fromJSON (builtins.readFile (
              pkgs.runCommand "packageYaml"
                {
                  buildInputs = [ pkgs.tree pkgs.yaml2json ];
                }
                ''
                  test -e ${configSource}/package.yaml
                  cat ${configSource}/package.yaml | yaml2json > $out
                ''));
            packageName = packageYaml.name;
          in
          rec {
            packages = {
              default = packages."${packageName}";
              "${packageName}" = pkgs.haskellPackages.callCabal2nix packageName args.root { };
            };

            apps =
              let
                mkApp = name: config: {
                  type = "app";
                  program = lib.getExe packages.default;
                };
              in
              lib.attrsets.mapAttrs mkApp packageYaml.executables;

            devShells.default =
              pkgs.mkShell {
                packages = [
                  pkgs.haskellPackages.hpack
                ];
              };
          }
        );
    };
}
