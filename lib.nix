{ inputs }: args:
let
  lib = inputs.nixpkgs.lib;
  fs = lib.fileset;
  isConfigFile = file: file.name == "package.yaml";
  configSource = fs.toSource {
    root = args.root;
    fileset = fs.fileFilter isConfigFile args.root;
  };
in
inputs.flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
let
  pkgs = inputs.nixpkgs.legacyPackages.${system};
  packageYaml = builtins.fromJSON (builtins.readFile (
    pkgs.runCommand "packageYaml"
      {
        buildInputs = [ pkgs.tree pkgs.yaml2json ];
      }
      ''
        if ! test -e ${configSource}/package.yaml ; then
          echo "No package.yaml found in ${configSource}" >&2
          exit 1
        fi
        cat ${configSource}/package.yaml | yaml2json > $out
      ''));
  packageName = packageYaml.name;
in
rec {
  packages = {
    default = packages."${packageName}";
    "${packageName}" =
      let
        src = fs.toSource {
          root = args.root;
          fileset = fs.fileFilter
            (file: isConfigFile file || file.hasExt "hs")
            args.root;
        };
      in
      pkgs.haskellPackages.callCabal2nix packageName src { };
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
        (pkgs.haskell-language-server.override { dynamic = true; })
      ];
    };
})
