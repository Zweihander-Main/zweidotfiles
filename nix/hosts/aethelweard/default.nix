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

  # Disable faulty soldered SSD
  boot.kernelParams = ["libata.force=1.00:disable"];

  # Reduce writes on SD card
  services.journald.storage = "volatile";
  services.journald.extraConfig = "SystemMaxUse=20M";
  boot.tmp.useTmpfs = true;

  # Allow for udiskie
  services.udisks2.enable = true;

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
    acpilight
    findutils
    git
    git-crypt
    home-manager
    htop-vim
    killall
    lm_sensors
    lynx
    mlocate
    unstable.pcsclite # https://github.com/NixOS/nixpkgs/issues/290926
    pinentry-gtk2
    procps
    unzip
    upower
    vim
    w3m
    wget
    xorg.xev
    zsh
  ];

  fonts.packages = with pkgs; [
    noto-fonts
    source-code-pro
  ];

  programs.zsh = {
    enable = true;
    histFile = "$XDG_DATA_HOME/shell/history";
  };

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
    pinentryPackage = pkgs.pinentry-gtk2;
  };
  services.udev.packages = [pkgs.yubikey-personalization];
  services.pcscd.enable = true;

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
    dpi = 196;
    xkb.layout = "us";
    displayManager.startx.enable = true;
    monitorSection = ''
      DisplaySize 254 169
    '';
  };

  # Enable touchpad
  services.libinput.enable = true;

  # Enable sound
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    #jack.enable = true;
  };

  # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
  system.stateVersion = "23.05";
}
