# Per host common options used in both home manager and nixos
{
  lib,
  ...
}:
with lib; let
  cfg = config.hostAttr;
in {
  options.hostAttr = {
    ddcci = mkOption {
      type = types.bool;
      default = false;
      description = "If the monitor setup supports DDC/CI controls for backlight control.";
    };
  };
}
