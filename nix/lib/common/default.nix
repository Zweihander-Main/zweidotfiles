# Per host common options used in both home manager and nixos
{lib, ...}:
with lib; let
  cfg = config.hostAttr;
in {
  options.hostAttr = {
    monitor = {
      ddcci = mkOption {
        type = types.bool;
        default = false;
        description = "If the monitor setup supports DDC/CI controls for backlight control.";
      };
      nightTemp = mkOption {
        type = types.int;
        default = 1200;
        description = "Color temp to use at night, between`1000` and `25000` K.";
      };
    };
  };
}