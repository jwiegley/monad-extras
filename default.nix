{ mkDerivation, base, mmorph, monad-control, stdenv, stm
, transformers, transformers-base
}:
mkDerivation {
  pname = "monad-extras";
  version = "0.5.10";
  src = ./.;
  buildDepends = [
    base mmorph monad-control stm transformers transformers-base
  ];
  homepage = "http://github.com/jwiegley/monad-extras";
  description = "Extra utility functions for working with monads";
  license = stdenv.lib.licenses.bsd3;
}
