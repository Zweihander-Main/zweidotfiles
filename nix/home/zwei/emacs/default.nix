{
  pkgs,
  config,
  lib,
  ...
}:
with lib; let
  homeDir = config.home.homeDirectory;
  doomRepoUrl = "https://github.com/doomemacs/doomemacs";
  configRepoUrl = "https://github.com/Zweihander-Main/zweidoom";
in {
  imports = [
    # :checkers spell
    ../aspell
  ];

  programs.emacs = mkIf (!config.hostAttr.preinstalled.emacs) {
    enable = true;
    package = pkgs.emacs;
  };

  services.emacs = mkIf (!config.hostAttr.preinstalled.emacs) {
    enable = true;
    package = pkgs.emacs;
  };

  systemd.user.services.emacs = mkIf (!config.hostAttr.preinstalled.emacs) {
    Install = mkForce {WantedBy = ["wm.target"];};
  };

  xdg.configFile."systemd/user/emacs.service.d/override.conf".source =
    ./override.conf;

  fonts.fontconfig.enable = true;

  home.packages = with pkgs;
    mkIf (!config.hostAttr.preinstalled.emacs) [
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
      iosevka-ss09
      iosevka-term-ss09
      iosevka-aile

      ## Personal config deps
      lsb-release

      ## Module dependencies
      # :lang cc
      # libclang
      glslang
      # :lang common-lisp
      sbcl
      # :lang data
      libxml2
      # :lang markdown
      python311Packages.grip
      pandoc
      # lang org
      xclip
      maim
      graphviz
      # :lang sh
      shfmt
      shellcheck
      # :lang web
      html-tidy
      stylelint
      nodePackages.js-beautify
      # :lang zsh
      beautysh
      # :lang nix
      nixfmt-classic
      nil
      # :tools lookup & :lang org +roam
      sqlite
      # :tools ansible
      ansible
      # :tools docker
      dockfmt
      # :tools editoconfig
      editorconfig-core-c
      # :tools copilot
      nodePackages.npm
    ];

  home.sessionPath = ["$XDG_CONFIG_HOME/emacs/bin"];

  home.activation = {
    installDoomEmacs = ''
      if [ ! -d "$XDG_CONFIG_HOME/emacs" ]; then
        ${pkgs.git}/bin/git clone --depth=1 --single-branch "${doomRepoUrl}" "$XDG_CONFIG_HOME/emacs"
      fi
      if [ ! -f "$XDG_CONFIG_HOME/doom/init.el" ]; then
        mkdir -p "$XDG_CONFIG_HOME/doom"
        cd "$XDG_CONFIG_HOME/doom"
        ${pkgs.git}/bin/git init
        ${pkgs.git}/bin/git remote add origin "${configRepoUrl}"
        ${pkgs.git}/bin/git fetch
        ${pkgs.git}/bin/git checkout origin/master -ft
        mkdir -p "$XDG_CONFIG_HOME/doom/snippets"
        PATH="$XDG_STATE_HOME/nix/profile/bin:$PATH"
        cd "$XDG_CONFIG_HOME"/emacs/bin/
        ./doom install
      else
        PATH="$XDG_STATE_HOME/nix/profile/bin:$PATH"
        cd "$XDG_CONFIG_HOME/doom"
        ${pkgs.git}/bin/git pull
        cd "$XDG_CONFIG_HOME"/emacs/bin/
        ./doom sync
      fi
      if [ ! -d "${homeDir}/org" ]; then
        mkdir -p "${homeDir}/org/gtd"
      fi
    '';
  };
}
