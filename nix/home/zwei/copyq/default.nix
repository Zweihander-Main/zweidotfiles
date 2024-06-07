{
  pkgs,
  config,
  lib,
  ...
}: {
  home.packages = with pkgs; [
    copyq
  ];

  # Config symlinked by chezmoi

  systemd.user.services.copyq = {
    Unit = {
      Description = "Copyq clipboard manager";
      Documentation = "man:copyq(1)";
      Wants = ["tray.service"];
      After = ["tray.service"];
      ConditionFileIsExecutable = "${pkgs.copyq}/bin/copyq";
    };
    Service = {
      Type = "exec";
      RestartSec = "20s";
      Restart = "always";
      ExecStart = "${pkgs.copyq}/bin/copyq";
    };
    Install = {
      WantedBy = ["wm.target"];
    };
  };
}
