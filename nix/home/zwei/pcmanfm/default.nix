{
  pkgs,
  config,
  lib,
  secrets,
  ...
}: {
  home.packages = with pkgs; [
    pcmanfm
  ];

  systemd.user.services.pcmanfm = {
    Unit = {
      Description = "PCManFM daemon mode";
      Documentation = "pcmanfm(1)";
      ConditionFileIsExecutable = ["${pkgs.pcmanfm}/bin/pcmanfm"];
    };

    Service = {
      Type = "simple";
      ExecStart = "${pkgs.pcmanfm}/bin/pcmanfm -d";
      Restart = "always";
      RestartSec = "10s";
    };

    Install = {WantedBy = ["wm.target"];};
  };
}
