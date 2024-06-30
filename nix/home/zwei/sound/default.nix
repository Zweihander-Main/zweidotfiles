{
  pkgs,
  config,
  lib,
  secrets,
  ...
}: {
  home.packages = with pkgs; [
    pavucontrol
    pa-notify
  ];

  xdg.configFile."systemd/user/pipewire-pulse.service/override.conf".source = ./override.conf;
  # xdg.configFile."systemd/user/sound.service".source = config.lib.file.mkOutOfStoreSymlink "${config.hostAttr.paths.systemdUserPkgServiceFiles}/pipewire-pulse.service";
  # xdg.configFile."systemd/user/default.target.wants/pipewire-pulse.service".source = config.lib.file.mkOutOfStoreSymlink "${config.hostAttr.paths.systemdUserPkgServiceFiles}/pipewire-pulse.service";
  # xdg.configFile."systemd/user/sockets.target.wants/pipewire-pulse.socket".source = config.lib.file.mkOutOfStoreSymlink "${config.hostAttr.paths.systemdUserPkgServiceFiles}/pipewire-pulse.socket";

  systemd.user.services.volume-notification = {
    Unit = {
      Description = "Volume notification (by pa-notify)";
      Requires = ["sound.service"];
      Wants = ["dunst.service"];
      After = ["dunst.service"];
      ConditionFileIsExecutable = ["${pkgs.pa-notify}/bin/pa-notify"];
    };

    Service = {
      Type = "exec";
      Restart = "always";
      RestartSec = "10s";
      ExecStartPre = "${pkgs.runtimeShell} -c 'exec sleep 2'";
      ExecStart = "${pkgs.pa-notify}/bin/pa-notify";
    };

    Install = {WantedBy = ["wm.target"];};
  };
}
