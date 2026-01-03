let
  nixpkgs = import <nixpkgs> {};
  pkgs = nixpkgs.pkgs;
  gitignoreSource = pkgs.nix-gitignore.gitignoreSource;
in {
  audacity = pkgs.callPackage ./audacity.nix { inherit gitignoreSource; };
}
