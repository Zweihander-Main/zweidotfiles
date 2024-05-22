{
  pkgs,
  config,
  lib,
  secrets,
  ...
}: let
  homeDir = config.home.homeDirectory;
in {
  systemd.user.services.nix_logout = {
    Unit = {
      Description = "Force logout at given time";
    };

    Service = {
      Type = "oneshot";
      ExecStart = "${pkgs.procps}/bin/pkill -U zwei xinit";
    };
  };

  systemd.user.timers.nix_logout = {
    Unit = {
      Description = "Automatically logout at 1";
    };

    Timer = {
      OnCalendar = "1:00";
    };

    Install = {
      WantedBy = ["timers.target"];
    };
  };
}
