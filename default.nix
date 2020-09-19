let default = import (fetchTarball "https://nixos.org/channels/nixos-20.09/nixexprs.tar.xz") {}; in
{ nixpkgs ? default }:

nixpkgs.stdenv.mkDerivation {
  pname = "extol";
  version = "0.0.1";

  src = nixpkgs.lib.cleanSource ./.;

  nativeBuildInputs = [ nixpkgs.gprolog ];

  preBuild = "make configure PREFIX=$out";

  doCheck = true;
}
