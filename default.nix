{ haskellNix ? import (builtins.fetchTarball "https://github.com/input-output-hk/haskell.nix/archive/dc7acfeb7628032d5109747a8b330d58817b953f.tar.gz") {}
, nixpkgsSrc ? haskellNix.sources.nixpkgs-2003
, nixpkgsArgs ? haskellNix.nixpkgsArgs
, pkgs ? import nixpkgsSrc nixpkgsArgs
}: 

pkgs.haskell-nix.project {
  projectFileName = "stack-8.10.1.yaml";
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "opentelemetry-haskell";
    src = ./.;
  };
}
