{
  inputs,
  lib,
  config,
  pkgs,
  secrets,
  outputs,
  ...
}: {
  imports = [
    inputs.hardware.nixosModules.microsoft-surface-pro-3
    ./hardware-configuration.nix
    # ./lib.nix
  ];

  nixpkgs = {
    overlays = [
      outputs.overlays.additions
      outputs.overlays.modifications
      outputs.overlays.unstable-packages
    ];
    config = {
      allowUnfree = true;
    };
  };

  # This will add each flake input as a registry
  # To make nix3 commands consistent with your flake
  nix.registry = (lib.mapAttrs (_: flake: {inherit flake;})) ((lib.filterAttrs (_: lib.isType "flake")) inputs);

  # This will additionally add your inputs to the system's legacy channels
  # Making legacy nix commands consistent as well, awesome!
  nix.nixPath = ["/etc/nix/path"];
  environment.etc =
    lib.mapAttrs'
    (name: value: {
      name = "nix/path/${name}";
      value.source = value.flake;
    })
    config.nix.registry;

  nix.settings = {
    experimental-features = "nix-command flakes";
    auto-optimise-store = true;
    use-xdg-base-directories = true;
  };

  networking = {
    hostName = "aethelweard";
    wireless = {
      enable = true; # Enables wireless support via wpa_supplicant.
      networks = {
        "${secrets.wifi.ssid}" = {
          hidden = secrets.wifi.hidden;
          psk = "${secrets.wifi.password}";
        };
      };
    };
    # Allows syncthing
    firewall = {
      allowedTCPPorts = [22000];
      allowedUDPPorts = [22000 21027];
    };
  };

  # Set your time zone.
  time.timeZone = "America/New_York";

  # Use the systemd-boot EFI  boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.kernelModules = ["hid-microsoft"];

  users.users = {
    zwei = {
      initialPassword = "changeme";
      isNormalUser = true;
      openssh.authorizedKeys.keys = ["${secrets.ssh.authorized_key}"];
      shell = pkgs.zsh;
      extraGroups = ["wheel"];
    };
    karlmagnus = {
      initialPassword = "changeme";
      isNormalUser = true;
      openssh.authorizedKeys.keys = ["${secrets.ssh.authorized_key}"];
      shell = pkgs.zsh;
    };
  };

  security.sudo = {
    enable = true;
    extraRules = [
      {
        commands = [
          {
            command = "/run/current-system/sw/bin/xbacklight";
            options = ["NOPASSWD"];
          }
        ];
        groups = ["wheel"];
      }
    ];
  };

  environment.systemPackages = with pkgs; [
    vim
    git
    git-crypt
    findutils
    mlocate
    wget
    w3m
    lynx
    htop-vim
    zsh
    home-manager
    procps
    killall
    acpilight
    upower
    lm_sensors
  ];

  fonts.packages = with pkgs; [
    noto-fonts
    source-code-pro
  ];

  programs.zsh.enable = true;

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  # Enable the OpenSSH daemon.
  services.openssh = {
    enable = true;
    settings = {
      PermitRootLogin = "no";
      PasswordAuthentication = false;
    };
  };

  services.locate = {
    enable = true;
    package = pkgs.mlocate;
    localuser = null;
  };

  services.upower = {
    enable = true;
    package = pkgs.upower;
  };

  # Enable the X11 windowing system.
  services.xserver = {
    enable = true;
    layout = "us";
    libinput.enable = true; #touchpad
    displayManager.startx.enable = true;
  };

  # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
  system.stateVersion = "23.05";
}
