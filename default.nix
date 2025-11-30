let
  locked = import ./lock.nix;
  pkgs = import locked.nixpkgs {};
in with pkgs; with pkgs.lib; let

  extol = stdenv.mkDerivation {
    pname = "extol";
    version = "0.0.5";
    src = ./.;
    nativeBuildInputs = [ gprolog mlton ];
    configurePhase = "make configure PREFIX=$out";
    doCheck = true;
  };

in {
  inherit extol;
}
