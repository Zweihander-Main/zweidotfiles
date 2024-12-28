{ inputs, lib, config, pkgs, secrets, outputs, ... }: {
  nixpkgs = {
    overlays = [
      outputs.overlays.additions
      outputs.overlays.modifications
      outputs.overlays.unstable-packages
    ];
    config = { allowUnfree = true; };
  };

  nix.registry = (lib.mapAttrs (_: flake: { inherit flake; }))
    ((lib.filterAttrs (_: lib.isType "flake")) inputs);
  nix.nixPath = [ "/etc/nix/path" ];
  environment.etc = lib.mapAttrs' (name: value: {
    name = "nix/path/${name}";
    value.source = value.flake;
  }) config.nix.registry;

  nix.settings = {
    experimental-features = "nix-command flakes";
    auto-optimise-store = true;
    use-xdg-base-directories = true;
  };

  environment.packages = with pkgs; [
    # Nix
    home-manager
    # Basic packages
    bzip2
    chezmoi
    curl
    diffutils
    file
    findutils
    gawk
    git
    git-crypt
    gnugrep
    gnupg
    gnused
    gnutar
    gzip
    hostname
    killall
    lsb-release
    man
    openssh
    procps
    tzdata
    unzip
    utillinux
    vim
    vimpager
    wget
    which
    xz
    zip
    zsh
  ];

  # Backup etc files instead of failing to activate generation if a file already exists in /etc
  environment.etcBackupExtension = ".bak";

  user.shell = "${pkgs.zsh}/bin/zsh";

  time.timeZone = "America/New_York";

  system.stateVersion = "23.05";
}
# vim: ft=nix

