{
  description = "syncbox";

  inputs = {
    nixpkgs = { url = "github:NixOS/nixpkgs/release-22.11"; };
    nixops-plugged = { url = "github:lukebfox/nixops-plugged"; };
    nix-filter.url = "github:numtide/nix-filter";
    flake-utils = { url = "github:numtide/flake-utils"; };
    streamly = {
      url = "github:composewell/streamly";
      flake = false;
    };
    streamly-process = {
      url = "github:composewell/streamly-process";
      flake = false;
    };
  };

  outputs =
    { self
    , nixpkgs
    , flake-utils
    , nix-filter
    , ...
    }:
    let
      pkgsFor = system:
        import nixpkgs {
          inherit system;
          overlays = [ self.overlays.${system} nix-filter.overlays.default ];
        };
    in
    flake-utils.lib.eachDefaultSystem
      (
        system:
        let
          pkgs = pkgsFor system;
          filteredSrc =
            pkgs.nix-filter {
              root = ./.;
              include = [
                "src/"
                "test/"
                "package.yaml"
                "LICENSE"
              ];
            };
        in
        rec {
          overlays = final: prev: {
            haskellPackages = prev.haskellPackages.override (old: {
              overrides = final.lib.composeExtensions (old.overrides or (_: _: { }))
                (self: super: {
                    syncbox = self.generateOptparseApplicativeCompletions
                      [ "syncbox" ]
                      (self.callCabal2nix "syncbox" filteredSrc { });
                  }
                );
            });
          };
          packages = {
            syncbox =
              pkgs.haskell.lib.justStaticExecutables (
                pkgs.haskellPackages.syncbox.overrideAttrs
                  (oldAttrs: {
                    nativeBuildInputs = oldAttrs.nativeBuildInputs
                    ++ [ pkgs.makeWrapper ];
                    postInstall = (oldAttrs.postInstall or "") + ''
                      wrapProgram $out/bin/syncbox \
                        --suffix PATH : ${pkgs.lib.makeBinPath [pkgs.github-cli]}
                    '';
                  }
                  ));
          };

          defaultPackage = packages.syncbox;

          devShells.default = pkgs.haskellPackages.shellFor {
            packages = p: [ packages.syncbox ];
            buildInputs = with pkgs; with pkgs.haskellPackages; [
              haskell-language-server
              cabal-install
              ghcid
              hpack
              hlint
              yamlfix
            ];
          };
        }
      )
    // {
      nixopsConfigurations.default =
        let
          system = "x86_64-linux";
          syncbox = self.packages.${system}.syncbox;
          region = "eu-west-1";
          accessKeyId = "fake";
        in
        {
          inherit nixpkgs;
          network.description = "syncbox.com";
          resources = {
            ec2KeyPairs.syncbox-keys = { inherit region accessKeyId; };
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
          syncbox =
            { resources
            , pkgs
            , ...
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
              networking.firewall.allowedTCPPorts = [ 22 80 443 8000 8384 22000 22067 22070 ];
              networking.firewall.allowedUDPPorts = [ 21027 22000 ];
              networking.enableIPv6 = true;

              environment.systemPackages = [ pkgs.bash pkgs.fish ];

              nix.gc.automatic = true;

              users.users.damian = {
                isNormalUser = true;
                createHome = true;
                extraGroups = [ "wheel" ];
                group = "users";
              };

              services.openssh.extraConfig = ''
                MaxAuthTries 40
              '';

              services.cfdyndns = {
                enable = true;
                apikeyFile = ''${pkgs.writeText "cloudflare-apikey" "fake"}'';
                email = "huevofritopamojarpan@gmail.com";
                records = [ "fake-url" ];
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
                        loadBalancer.servers = [{ url = "http://localhost:3003"; }];
                      };
                    };
                    syncbox-router = {
                      router4 = {
                        service = "syncbox";
                        rule = "Host(`fake.com`)";
                        entryPoints = [ "http" ];
                      };
                    };
                  };
                };
              };

              systemd.services.syncbox = {
                description = "syncbox";
                wantedBy = [ "multi-user.target" ];
                enable = true;

                after = [ "network.target" ];
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
  nixConfig = {
    extra-substituters = "https://opensource.cachix.org";
    extra-trusted-public-keys = "opensource.cachix.org-1:6t9YnrHI+t4lUilDKP2sNvmFA9LCKdShfrtwPqj2vKc=";
  };
}
