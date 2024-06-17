{
  pkgs,
  config,
  lib,
  secrets,
  ...
}: {
  home.packages = with pkgs; [
    ungoogled-chromium
    profile-sync-daemon
  ];

  # For profile to RAM
  services.psd = {
    enable = true;
    resyncTimer = "1h";
  };
  xdg.configFile."psd/psd.conf".source = ./psd.conf;

  ## Temporary fix waiting on upstream
  systemd.user = {
    services = let
      exe = "${pkgs.profile-sync-daemon}/bin/profile-sync-daemon";
      envPath = lib.makeBinPath (with pkgs; [
        rsync
        kmod
        findutils
        gawk
        gnugrep
        gnused
        coreutils
        nettools
        util-linux
        profile-sync-daemon
      ]);
    in {
      psd = {
        Service = lib.mkForce {
          Type = "oneshot";
          RemainAfterExit = "yes";
          ExecStart = "${exe} startup";
          ExecStop = "${exe} unsync";
          Environment = ["LAUNCHED_BY_SYSTEMD=1" "PATH=$PATH:${envPath}"];
        };
      };

      psd-resync = {
        Service = lib.mkForce {
          Type = "oneshot";
          ExecStart = "${exe} resync";
          Environment = ["PATH=$PATH:${envPath}"];
        };
      };
    };
  };

  # For cache to RAM
  systemd.user.tmpfiles.rules = ["d /run/user/1000/chromium-cache"];
  home.file.".cache/chromium".source = config.lib.file.mkOutOfStoreSymlink "/run/user/1000/chromium-cache";
}
