{ mkDerivation, base, bytestring, HTF, lib, megaparsec, mtl
, unordered-containers
}:
mkDerivation {
  pname = "icfpc2024";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring megaparsec mtl unordered-containers
  ];
  executableHaskellDepends = [
    base bytestring megaparsec mtl unordered-containers
  ];
  testHaskellDepends = [
    base bytestring HTF megaparsec mtl unordered-containers
  ];
  homepage = "https://github.com/icfpc-dniwe/icfpc2024#readme";
  license = lib.licenses.bsd3;
}
