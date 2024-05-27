{
  config,
  pkgs,
  lib,
  ...
}:
with lib; let
  cfg = config.redshift;
in {
  options.redshift = {
    enable = mkOption {
      type = types.bool;
      default = false;
      description = ''
        Install redshift and enable service.
      '';
    };
    night = mkOption {
      type = types.int;
      default = 1200;
      description = ''
        Colour temperature to use at night, between `1000` and `25000` K.
      '';
    };
  };
  config = mkIf cfg.enable {
    services.redshift = {
      enable = true;
      provider = "manual";
      temperature = {
        day = 6700;
        night = cfg.night;
      };
      dawnTime = "9:00-10:45";
      duskTime = "22:00-23:45";
      latitude = "41.7";
      longitude = "-115";
      settings = {
        redshift = {
          brightness-night = "1";
          brightness-day = "0.8";
          fade = 1;
          adjustment-method = "randr";
        };
      };
    };

    xdg.configFile."redshift/hooks/brightness.sh".source = ./brightness.sh;
    xdg.configFile."systemd/user/wm.target.wants/redshift.service".text = mkIf config.hostAttr.ddcci "";
  };
}
