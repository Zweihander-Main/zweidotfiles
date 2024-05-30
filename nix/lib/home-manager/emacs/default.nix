{
  pkgs,
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.emacs;
  homeDir = config.home.homeDirectory;
in {
  options.emacs = {
    enable = mkOption {
      type = types.bool;
      default = false;
      description = ''
        Install emacs and service
      '';
    };
    doom = rec {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Install Doom framework
        '';
      };
      forgeUrl = mkOption {
        type = types.str;
        default = "https://github.com";
      };
      repoUrl = mkOption {
        type = types.str;
        default = "${cfg.doom.forgeUrl}/doomemacs/doomemacs";
      };
      configRepoUrl = mkOption {
        type = types.str;
        default = "${cfg.doom.forgeUrl}/Zweihander-main/zweidoom";
      };
    };
  };

  config = mkIf cfg.enable {
    programs.emacs = {
      enable = true;
      package = pkgs.emacs;
    };

    services.emacs = {
      enable = true;
      package = pkgs.emacs;
    };

    fonts.fontconfig.enable = true;

    home.packages = with pkgs; [
      ## Doom dependencies
      git
      (ripgrep.override {withPCRE2 = true;})
      nodejs-slim

      ## Optional dependencies
      fd # faster projectile indexing
      zstd # for undo-fu-session/undo-tree compression

      ## Fonts
      emacs-all-the-icons-fonts
      (nerdfonts.override {fonts = ["NerdFontsSymbolsOnly"];})
    ];

    home.sessionPath = ["$XDG_CONFIG_HOME/emacs/bin"];

    home.activation = mkIf cfg.doom.enable {
      installDoomEmacs = ''
        if [ ! -d "$XDG_CONFIG_HOME/emacs" ]; then
           ${pkgs.git}/bin/git clone --depth=1 --single-branch "${cfg.doom.repoUrl}" "$XDG_CONFIG_HOME/emacs"
        fi
        if [ ! -f "$XDG_CONFIG_HOME/doom/init.el" ]; then
           mkdir -p "$XDG_CONFIG_HOME/doom"
           cd "$XDG_CONFIG_HOME/doom"
           ${pkgs.git}/bin/git init
           ${pkgs.git}/bin/git remote add origin "${cfg.doom.configRepoUrl}"
           ${pkgs.git}/bin/git fetch
           ${pkgs.git}/bin/git checkout origin/master -ft
           cd "$XDG_CONFIG_HOME"/emacs/bin/
           ./doom install
        fi
        if [ ! -d "${homeDir}/org" ]; then
          mkdir -p "${homeDir}/org"
        fi
      '';
    };
  };
}
