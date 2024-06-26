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

  systemd.user.services.autolock-reminder = {
    Unit = {
      Description = "Notify about upcoming autolock";
      ConditionFileIsExecutable = "${pkgs.dunst}/bin/dunstify";
    };

    Service = {
      Type = "oneshot";
      ExecStart = "dunstify -a \"autolock\" -i appointment-soon \"Autolock coming up\"";
    };
  };

  systemd.user.timers.autolock-reminder = {
    Unit = {
      Description = "Notify about autolock at 16:25, 16:35";
    };

    Timer = {
      OnCalendar = "16:25,35";
    };

    Install = {
      WantedBy = ["timers.target"];
    };
  };

  systemd.user.services.autolock = {
    Unit = {
      Description = "Lock computer, checking if it's already locked";
      ConditionFileIsExecutable = "%h/.local/bin/check_i3lock";
    };

    Service = {
      Type = "oneshot";
      ExecStart = "%h/.local/bin/check_i3lock";
    };
  };

  systemd.user.timers.autolock = {
    Unit = {
      Description = "Automatically lock system between 16:30-16:45";
    };

    Timer = {
      OnCalendar = "16:30..45:0,15,30,45";
      AccuracySec= "5s";
    };

    Install = {
      WantedBy = ["timers.target"];
    };
  };
}
