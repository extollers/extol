{
  description = "The Extol programming language";

  inputs.nixpkgs.url = github:NixOS/nixpkgs/nixos-23.05;

  outputs = { self, nixpkgs }: let

    version = "0.0.1";

    extol = pkgs: pkgs.callPackage ({stdenv, gprolog}:
      stdenv.mkDerivation {
        pname = "extol";
        inherit version;
        src = self;
        nativeBuildInputs = [ gprolog ];
        configurePhase = "make configure PREFIX=$out";
        doCheck = false; # TODO
      }) {};

    packages = each ({pkgs, packages, ...}: {
      extol = extol pkgs;
      default = packages.extol;
    });

    checks = each ({pkgs, packages, ...}: {
      inherit (packages) extol;
    });

    systems = ["x86_64-linux" "i686-linux"];

    each = f: builtins.listToAttrs (map (system: {
      name = system;
      value = f {
        packages = packages.${system};
        pkgs = nixpkgs.legacyPackages.${system};
      };
    }) systems);

  in  {
    inherit packages;
    inherit checks;
  };
}
