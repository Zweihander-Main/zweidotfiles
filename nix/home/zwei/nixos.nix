{
  inputs,
  lib,
  config,
  pkgs,
  outputs,
  secrets,
  ...
}: {
  imports =
    [
      ./chezmoi
      ./syncthing
      ./time
      ./x11
      ./coreutils
      ./redshift
      ./stalonetray
      ./lf
    ]
    ++ (builtins.attrValues outputs.homeManagerModules);

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

  emacs = {
    enable = true;
    doom.enable = true;
  };

  # Nicely reload system units when changing configs
  systemd.user.startServices = "sd-switch";

  # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
  home.stateVersion = "23.05";
}
