{
  description = "syncbox";

  inputs = {
    nixpkgs = {url = "github:NixOS/nixpkgs/master";};
    nixops-plugged = {url = "github:lukebfox/nixops-plugged";};
    mne = {url = "github:damianfral/merge-nix-envs";};
    flake-utils = {url = "github:numtide/flake-utils";};
    streamly = {
      url = "github:composewell/streamly";
      flake = false;
    };
    streamly-process = {
      url = "github:composewell/streamly-process";
      flake = false;
    };
  };

  outputs = {
    self,
    nixpkgs,
    mne,
    flake-utils,
    nixops-plugged,
    ...
  } @ inputs: let
    pkgsFor = system:
      import nixpkgs {
        inherit system;
        overlays = [];
      };
  in
    flake-utils.lib.eachDefaultSystem
    (
      system: let
        pkgs = pkgsFor system;
        mk-syncbox = returnShellEnv:
          pkgs.haskellPackages.developPackage {
            root = ./.;
            source-overrides = {};
            overrides = new: old: let
              dontCheck = pkgs.haskell.lib.dontCheck;
              unmarkBroken = pkgs.haskell.lib.unmarkBroken;
            in {
              streamly = old.callCabal2nix "streamly" inputs.streamly {};
              streamly-core = old.callCabal2nix "streamly-core" "${inputs.streamly}/core" {};
              streamly-process = dontCheck (old.callCabal2nix "streamly-process" inputs.streamly-process {});
            };
            modifier = drv:
              if returnShellEnv
              then
                pkgs.haskell.lib.addBuildTools drv
                (
                  with pkgs.haskellPackages; [
                    cabal-install
                    ghcid
                    haskell-language-server
                    hlint
                    ormolu
                  ]
                )
              else pkgs.haskell.lib.justStaticExecutables drv;
            inherit returnShellEnv;
          };
      in rec {
        packages = rec {
          syncbox = mk-syncbox false;
          syncbox-shell = mk-syncbox true;

          dev-shell = pkgs.mkShell {
            buildInputs = with pkgs; [alejandra];
          };
        };
        defaultPackage = packages.syncbox;

        devShell = with packages;
          mne.lib.mergeNixEnvs
          [syncbox-shell dev-shell];
      }
    )
    // {
      nixopsConfigurations.default = let
        system = "x86_64-linux";
        pkgs = pkgsFor system;
        syncbox = self.packages.${system}.syncbox;
        region = "eu-west-1";
        accessKeyId = "fake";
      in {
        inherit nixpkgs;
        network.description = "syncbox.com";
        resources = {
          ec2KeyPairs.syncbox-keys = {inherit region accessKeyId;};
          ec2SecurityGroups.ssh-http-https = {
            inherit region accessKeyId;
            name = "ssh-http-https";
            description = "syncbox security group";
            rules = [
              {
                fromPort = 22;
                toPort = 22;
                sourceIp = "0.0.0.0/0";
              }
              {
                fromPort = 80;
                toPort = 80;
                sourceIp = "0.0.0.0/0";
              }
              {
                fromPort = 443;
                toPort = 443;
                sourceIp = "0.0.0.0/0";
              }
            ];
          };
        };
        syncbox = {
          resources,
          pkgs,
          ...
        }: {
          deployment = {
            targetEnv = "ec2";
            ec2 = {
              accessKeyId = accessKeyId;
              region = region;
              instanceType = "t3a.nano";
              keyPair = resources.ec2KeyPairs.syncbox-keys;
              # ebsInitialRootDiskSize = 20;
              # ebsBoot = true;
              associatePublicIpAddress = true;
              securityGroups = [
                resources.ec2SecurityGroups.ssh-http-https
              ];
            };
          };

          networking.hostName = "syncbox";
          networking.firewall.enable = false;
          networking.firewall.allowedTCPPorts = [22 80 443 8000 8384 22000 22067 22070];
          networking.firewall.allowedUDPPorts = [21027 22000];
          networking.enableIPv6 = true;

          environment.systemPackages = [pkgs.bash pkgs.fish];

          nix.gc.automatic = true;

          users.users.damian = {
            isNormalUser = true;
            createHome = true;
            extraGroups = ["wheel"];
            group = "users";
          };

          services.openssh.extraConfig = ''
            MaxAuthTries 40
          '';

          services.cfdyndns = {
            enable = true;
            apikeyFile = ''${pkgs.writeText "cloudflare-apikey" "fake"}'';
            email = "huevofritopamojarpan@gmail.com";
            records = ["fake-url"];
          };

          services.syncthing = {
            enable = true;
            relay.enable = true;
            openDefaultPorts = true;
          };

          services.traefik = {
            enable = true;
            staticConfigOptions = {
              entryPoints.http.address = ":80";
              pilot.token = "fake-token";
            };
            dynamicConfigOptions = {
              http = {
                services = {
                  syncbox-server = {
                    loadBalancer.servers = [{url = "http://localhost:3003";}];
                  };
                };
                syncbox-router = {
                  router4 = {
                    service = "syncbox";
                    rule = "Host(`fake.com`)";
                    entryPoints = ["http"];
                  };
                };
              };
            };
          };

          systemd.services.syncbox = {
            description = "syncbox";
            wantedBy = ["multi-user.target"];
            enable = true;

            after = ["network.target"];
            serviceConfig = {
              StandardOutput = "journal+console";
              ExecStart = ''
                ${syncbox}/bin/syncbox --port 3003 --root /var/lib/syncthing/Public
              '';
            };
          };
        };
      };
    };
}
