{ mkDerivation, stdenv }:
mkDerivation {
  pname = "ask-for";
  version = "0.1.0.0";
  src = ./.;
  license = stdenv.lib.licenses.mit;
}
