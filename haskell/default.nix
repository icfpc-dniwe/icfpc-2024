{ mkDerivation, base, bytestring, dotenv, HTF, lib, megaparsec
, monad-st, mtl, unordered-containers, wreq
}:
mkDerivation {
  pname = "icfpc2024";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring megaparsec monad-st mtl unordered-containers
  ];
  executableHaskellDepends = [
    base bytestring dotenv megaparsec monad-st mtl unordered-containers
    wreq
  ];
  testHaskellDepends = [
    base bytestring HTF megaparsec monad-st mtl unordered-containers
  ];
  homepage = "https://github.com/icfpc-dniwe/icfpc2024#readme";
  license = lib.licenses.bsd3;
}
