{
  description = "syncbox";

  inputs = {
    nixpkgs = {url = "github:NixOS/nixpkgs/release-24.11";};
    nix-filter.url = "github:numtide/nix-filter";
    flake-utils = {url = "github:numtide/flake-utils";};
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    pre-commit-hooks.inputs.nixpkgs.follows = "nixpkgs";
    weeder-nix.url = "github:NorfairKing/weeder-nix";
    weeder-nix.inputs = {
      nixpkgs.follows = "nixpkgs";
      pre-commit-hooks.follows = "pre-commit-hooks";
    };
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    nix-filter,
    pre-commit-hooks,
    ...
  } @ inputs: let
    pkgsFor = system:
      import nixpkgs {
        inherit system;
        overlays = [self.overlays.${system} nix-filter.overlays.default];
      };
  in
    flake-utils.lib.eachDefaultSystem
    (
      system: let
        pkgs = pkgsFor system;
        filteredSrc = pkgs.nix-filter {
          root = ./.;
          include = [
            "src/"
            "test/"
            "package.yaml"
            "LICENSE"
          ];
        };
        precommitCheck = pre-commit-hooks.lib.${system}.run {
          src = ./.;
          hooks = {
            actionlint.enable = true;
            alejandra.enable = true;
            deadnix.enable = true;
            hlint.enable = true;
            hpack.enable = true;
            markdownlint.enable = true;
            nil.enable = true;
            ormolu.enable = true;
          };
        };
      in rec {
        overlays = final: prev: {
          haskellPackages = prev.haskellPackages.override (old: {
            overrides =
              final.lib.composeExtensions (old.overrides or (_: _: {}))
              (
                self: _super: {
                  syncbox =
                    self.generateOptparseApplicativeCompletions
                    ["syncbox"]
                    (self.callCabal2nix "syncbox" filteredSrc {});
                }
              );
          });
        };
        packages = rec {
          default = syncbox;
          syncbox = pkgs.haskell.lib.justStaticExecutables (
            pkgs.haskellPackages.syncbox.overrideAttrs
            (
              oldAttrs: {
                nativeBuildInputs =
                  oldAttrs.nativeBuildInputs
                  ++ [pkgs.makeWrapper];
                postInstall =
                  (oldAttrs.postInstall or "")
                  + ''
                    wrapProgram $out/bin/syncbox \
                      --suffix PATH : ${pkgs.lib.makeBinPath [pkgs.github-cli]}
                  '';
              }
            )
          );
        };
        checks = {
          pre-commit-check = precommitCheck;
          weeder-check = inputs.weeder-nix.lib.${system}.makeWeederCheck {
            haskellPackages = pkgs.haskellPackages;
            packages = ["syncbox"];
            reportOnly = true;
          };
        };

        devShells.default = pkgs.haskellPackages.shellFor {
          packages = _p: [packages.syncbox];
          buildInputs = with pkgs;
          with pkgs.haskellPackages; [
            alejandra
            haskell-language-server
            cabal-install
            ghcid
            hpack
            hlint
            yamlfix
          ];
          shellHook = precommitCheck.shellHook;
        };
      }
    );
  nixConfig = {
    extra-substituters = "https://opensource.cachix.org";
    extra-trusted-public-keys = "opensource.cachix.org-1:6t9YnrHI+t4lUilDKP2sNvmFA9LCKdShfrtwPqj2vKc=";
  };
}
