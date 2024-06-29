{
  poetry2nix,
  python3,
  ruff,
  pyright,
}:
poetry2nix.mkPoetryApplication {
  projectDir = ./.;
  python = python3;
  nativeBuildInputs = [ruff pyright];
}
