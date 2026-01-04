{
  description = "Flake to manage audacity dependencies";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";

    # This enables using the muse-framework as a submodule--otherwise, we would need to manually clone it from GitHub.
    muse-framework = {
      url = "git+file:./muse_framework?submodules=1";
      flake = false;
    };
  };

  outputs = {
    nixpkgs,
    flake-utils,
    muse-framework,
    ...
  }:
    flake-utils.lib.eachDefaultSystem
    (
      system: let
        pkgs = nixpkgs.legacyPackages.${system};
      in {
        packages = rec {
          default = audacity;
          audacity = pkgs.callPackage ./au3/linux/packages/nix {
            inherit muse-framework;
          };
        };
      }
    );
}
