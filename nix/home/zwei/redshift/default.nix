{
  config,
  pkgs,
  lib,
  ...
}:
with lib; {
  services.redshift = {
    enable = true;
    provider = "manual";
    temperature = {
      day = 6700;
      night = config.hostAttr.monitor.nightTemp;
    };
    dawnTime = "9:00-10:45";
    duskTime = "22:00-23:45";
    latitude = "41.7";
    longitude = "-115";
    settings = {
      redshift = {
        brightness-day = "1";
        brightness-night = "0.8";
        fade = 1;
        adjustment-method = "randr";
      };
    };
  };

  systemd.user.services.redshift = {
    Install = mkForce {WantedBy = ["wm.target"];};
  };

  xdg.configFile."redshift/hooks/brightness.sh" = mkIf config.hostAttr.monitor.ddcci {
    source = ./brightness.sh;
    executable = true;
  };

  xdg.configFile."autostart/geoclue-demo-agent.desktop".source = ./geoclue-demo-agent.desktop;
}
