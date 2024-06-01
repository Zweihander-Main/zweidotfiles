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

  xdg.configFile."redshift/hooks/brightness.sh".source = ./brightness.sh;
  xdg.configFile."systemd/user/wm.target.wants/redshift.service" = mkIf config.hostAttr.monitor.ddcci {
    text = "";
    executable = true;
  };
}
