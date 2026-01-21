{ pkgs, secrets, ... }: {
  nix.distributedBuilds = true;
  nix.settings.builders-use-substitutes = true;

  programs.ssh.extraConfig = ''
    Host nixbuilder
      HostName ${secrets.hosts.nixbuilder.hostname}
      Port ${secrets.hosts.nixbuilder.port}
      User ${secrets.hosts.nixbuilder.user}
      IdentityFile /root/.ssh/remotebuild
  '';

  nix.buildMachines = [{
    hostName = "nixbuilder";
    system = pkgs.stdenv.hostPlatform.system;
    supportedFeatures = [ "nixos-test" "big-parallel" "kvm" ];
  }];
}
