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
    preinstalled = {
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
      vim = mkOption {
        type = types.bool;
        default = false;
        description = "Is vim available on the host?";
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


  # Jinja2 templating
  mkTemplate = src: params:
    pkgs.runCommand "template-${src}" {
      buildInputs = [pkgs.j2cli];
      passAsFile = [
        "paramsJson"
      ];
      paramsJson = builtins.toJSON params;
    }
    ''
      ${pkgs.j2cli}/bin/j2 -f json ${src} "$paramsJsonPath" > "$out"
    '';
