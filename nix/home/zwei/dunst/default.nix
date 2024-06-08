{
  pkgs,
  config,
  lib,
  secrets,
  ...
}: {
  # TODO: Use home manager options for dunst service

  home.packages = with pkgs; [
    dunst
  ];

  xdg.configFile."dunst/dunstrc".source = ./dunstrc;
  xdg.dataFile."dbus-1/services/org.knopwob.dunst.service".source = ./org.knopwob.dunst.service;

  systemd.user.services.dunst = {
    Unit = {
      Description = "Dunst notification manager";
      PartOf = ["graphical-session.target"];
      ConditionFileIsExecutable = ["${pkgs.dunst}/bin/dunst" "${pkgs.dunst}/bin/dunstctl"];
    };

    Service = {
      Type = "dbus";
      BusName = "org.freedesktop.Notifications";
      Restart = "always";
      ExecStart = "${pkgs.dunst}/bin/dunst";
      # Make sure notifications aren't paused on start
      ExecStartPost = ["${pkgs.runtimeShell} -c 'exec sleep 2'" "${pkgs.dunst}/bin/dunstctl set-paused false"];
    };

    Install = {WantedBy = ["wm.target"];};
  };
}
