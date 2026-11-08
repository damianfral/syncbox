{
  description = "Syncbox synchronization tool (Haskell)";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";
    nix-filter.url = "github:numtide/nix-filter";
    flake-utils.url = "github:numtide/flake-utils";
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
          include = ["src/" "test/" "assets/" "migrations/" "package.yaml" "LICENSE"];
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
                  syncbox = let
                    externalTools = with final; [imagemagick ffmpeg];
                  in
                    (self.generateOptparseApplicativeCompletions
                      ["syncbox"]
                      (self.callCabal2nix "syncbox" filteredSrc {}))
                    .overrideAttrs (oldAttrs: {
                      nativeBuildInputs = with final;
                        oldAttrs.nativeBuildInputs ++ [makeWrapper];
                      postInstall =
                        (oldAttrs.postInstall or "")
                        + ''
                          wrapProgram $out/bin/syncbox \
                            --suffix PATH : ${final.lib.makeBinPath externalTools}
                        '';
                    });
                }
              );
          });
        };
        packages = rec {
          default = syncbox;
          syncbox = pkgs.haskell.lib.justStaticExecutables (
            pkgs.haskellPackages.syncbox
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
    )
    // {
      nixosModules.default = {
        config,
        lib,
        pkgs,
        ...
      }: let
        cfg = config.services.syncbox;
      in {
        options.services.syncbox = {
          package = lib.mkOption {
            type = lib.types.package;
            default = self.packages.${pkgs.stdenv.hostPlatform.system}.syncbox;
            defaultText = lib.literalExpression "self.packages.\${pkgs.stdenv.hostPlatform.system}.syncbox";
            description = "The syncbox package to use.";
          };

          port = lib.mkOption {
            type = lib.types.port;
            default = 8080;
            description = "Port for syncbox to listen on.";
          };

          root = lib.mkOption {
            type = lib.types.str;
            description = "Directory to index and serve.";
            example = "/srv/syncbox";
          };

          hidden = lib.mkOption {
            type = lib.types.bool;
            default = false;
            description = "Include hidden files and directories.";
          };

          openFirewall = lib.mkOption {
            type = lib.types.bool;
            default = false;
            description = "Whether to open the firewall for the configured port.";
          };
        };

        config = {
          systemd.services.syncbox = {
            description = "Syncbox file browser";
            after = ["network.target"];
            wantedBy = ["multi-user.target"];
            path = [pkgs.imagemagick pkgs.ffmpeg];

            serviceConfig = {
              ExecStart =
                "${lib.getExe cfg.package} --port ${toString cfg.port} --root "
                + lib.escapeShellArg cfg.root
                + lib.optionalString cfg.hidden " --hidden";
              DynamicUser = true;
              CacheDirectory = "syncbox";
              Environment = "XDG_CACHE_HOME=%C";
              Restart = "on-failure";

              # Hardening
              NoNewPrivileges = true;
              PrivateTmp = true;
              PrivateDevices = true;
              ProtectHome = true;
              ProtectKernelTunables = true;
              ProtectKernelModules = true;
              ProtectControlGroups = true;
              RestrictSUIDSGID = true;
              RestrictNamespaces = true;
              LockPersonality = true;
              MemoryDenyWriteExecute = true;
              RestrictRealtime = true;
              SystemCallArchitectures = "native";
            };
          };

          networking.firewall.allowedTCPPorts = lib.mkIf cfg.openFirewall [cfg.port];
        };
      };

      nixosModules.syncbox = self.nixosModules.default;
    };
  nixConfig = {
    extra-substituters = "https://opensource.cachix.org";
    extra-trusted-public-keys = "opensource.cachix.org-1:6t9YnrHI+t4lUilDKP2sNvmFA9LCKdShfrtwPqj2vKc=";
  };
}
