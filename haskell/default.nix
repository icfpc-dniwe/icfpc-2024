{ mkDerivation, base, bytestring, dotenv, hashable, hpack, HTF
, http-client, interpolatedstring-perl6, lens, lib, megaparsec
, monad-st, mtl, process, text, unordered-containers, wreq
}:
mkDerivation {
  pname = "icfpc2024";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring hashable interpolatedstring-perl6 megaparsec
    monad-st mtl process text unordered-containers
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base bytestring dotenv hashable http-client
    interpolatedstring-perl6 lens megaparsec monad-st mtl process text
    unordered-containers wreq
  ];
  testHaskellDepends = [
    base bytestring hashable HTF interpolatedstring-perl6 megaparsec
    monad-st mtl process text unordered-containers
  ];
  prePatch = "hpack";
  homepage = "https://github.com/icfpc-dniwe/icfpc2024#readme";
  license = lib.licenses.bsd3;
}
