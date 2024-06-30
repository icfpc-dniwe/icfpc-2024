{ mkDerivation, aeson, base, bytestring, dotenv, hpack, HTF
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
    aeson base bytestring interpolatedstring-perl6 megaparsec monad-st
    mtl process text unordered-containers
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson base bytestring dotenv http-client interpolatedstring-perl6
    lens megaparsec monad-st mtl process text unordered-containers wreq
  ];
  testHaskellDepends = [
    aeson base bytestring HTF interpolatedstring-perl6 megaparsec
    monad-st mtl process text unordered-containers
  ];
  prePatch = "hpack";
  homepage = "https://github.com/icfpc-dniwe/icfpc2024#readme";
  license = lib.licenses.bsd3;
}
