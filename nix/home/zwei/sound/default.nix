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
    playerctl
  ];

  xdg.configFile."systemd/user/pipewire-pulse.service.d/override.conf".source = ./override.conf;
  home.activation = {
    linkSound = lib.hm.dag.entryAfter ["writeBoundary" "installPackages" "xdg"] ''
      ln -sf "${config.hostAttr.paths.systemdUserPkgServiceFiles}pipewire-pulse.service" "$XDG_CONFIG_HOME/systemd/user/sound.service"
    '';
  };

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
