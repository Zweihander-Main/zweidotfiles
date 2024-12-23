# Per host common options used in both home manager and nixos
{
  lib,
  config,
  inputs,
  ...
}:
with lib; let
  cfg = config.hostAttr;
in {
  options.hostAttr = {
    type = mkOption {
      type = types.str;
      default = "work";
      description = "Type of host, either `work` or `play`.";
    };
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
      pixelRatio = mkOption {
        type = types.int;
        default = 1;
        description = "Multiple for DPI adjustments.";
      };
    };
    preinstalled = {
      alacritty = mkOption {
        type = types.bool;
        default = false;
        description = "Is alacritty available on the host?";
      };
      emacs = mkOption {
        type = types.bool;
        default = false;
        description = "Is emacs available on the host?";
      };
      pipeViewer = mkOption {
        type = types.bool;
        default = false;
        description = "Is pipe-viewer available on the host?";
      };
      systemd = mkOption {
        type = types.bool;
        default = true;
        description = "Is systemd available on the host?";
      };
      udiskie = mkOption {
        type = types.bool;
        default = false;
        description = "Is udiskie available on the host?";
      };
      vim = mkOption {
        type = types.bool;
        default = false;
        description = "Is vim available on the host?";
      };

    };
    programs = {
      pipeViewer = {
        player = mkOption {
          type = types.str;
          default = "audioonly";
          description = "Which player defined in config to use.";
        };
      };
    };
    paths = {
      systemdUserPkgServiceFiles = mkOption {
        type = types.str;
        default = "/etc/systemd/user/";
        description = "Path where preinstalled systemd user service files should be.";
      };
    };
  };
}
