{
  description = "The Extol programming language";

  inputs.nixpkgs = {
    url = "https://nixos.org/channels/nixos-20.09/nixexprs.tar.xz";
    flake = false;
  };

  outputs = { self, nixpkgs }: let
    extol = pkgs: pkgs.stdenv.mkDerivation {
      pname = "extol";
      version = "0.0.1";
      src = self;
      nativeBuildInputs = [ pkgs.gprolog ];
      preBuild = "make configure PREFIX=$out";
      doCheck = true;
    };
    platforms = ["x86_64-linux" "i686-linux"];
    all = builtins.listToAttrs (map (name: {
      inherit name;
      value = extol (import "${nixpkgs}" { system = name; });
    }) platforms);
  in { defaultPackage = all; };
}
