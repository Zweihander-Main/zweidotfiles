{
  inputs,
  lib,
  config,
  pkgs,
  outputs,
  secrets,
  ...
}: {
  imports = [
    ./chezmoi.nix
    ./emacs.nix
    ./syncthing.nix
    ./time.nix
    ./x11.nix
    ./coreutils.nix
    ./redshift
    ./stalonetray
    ./lf
  ];

  nixpkgs = {
    overlays = [
      outputs.overlays.additions
      outputs.overlays.modifications
      outputs.overlays.unstable-packages
    ];
    config = {
      allowUnfree = true;
      allowUnfreePredicate = _: true;
    };
  };

  home = {
    username = "zwei";
    homeDirectory = "/home/zwei";
  };

  home.packages = with pkgs; [ 
    # user programs      
    alacritty
    tmux
    firefox
    ungoogled-chromium
    sysz
    zathura
    unzip
    sxhkd
    # dev
    gcc 
    python3
  ];

  programs.home-manager.enable = true;

  xdg.enable = true;

  # Nicely reload system units when changing configs
  systemd.user.startServices = "sd-switch";

  # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
  home.stateVersion = "23.05";
}

