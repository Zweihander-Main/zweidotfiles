{
  pkgs,
  config,
  lib,
  secrets,
  ...
}: let 
  homeDir = config.home.homeDirectory;
  forgeUrl = "https://github.com";
  repoUrl = "${forgeUrl}/doomemacs/doomemacs";
  configRepoUrl = "${forgeUrl}/Zweihander-Main/zweidoom";
in {

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
    fd                  # faster projectile indexing
    zstd                # for undo-fu-session/undo-tree compression

    ## Personal config deps
    lsb-release

    ## Module dependencies
    # :lang cc 
    libclang
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
    # :checkers spell
    (aspellWithDicts (ds: with ds; [ en en-computers en-science ]))
    # :tools lookup & :lang org +roam
    sqlite
    # :tools ansible
    ansible
    # :tools docker
    dockfmt
    # :tools editoconfig
    editorconfig-core-c

    # Fonts
    emacs-all-the-icons-fonts
    (nerdfonts.override { fonts = [ "NerdFontsSymbolsOnly" ]; })
  ];

  home.sessionPath = [ "$XDG_CONFIG_HOME/emacs/bin" ];

  home.activation = {
    installDoomEmacs = ''
      if [ ! -d "$XDG_CONFIG_HOME/emacs" ]; then
         ${pkgs.git}/bin/git clone --depth=1 --single-branch "${repoUrl}" "$XDG_CONFIG_HOME/emacs"
      fi
      if [ ! -f "$XDG_CONFIG_HOME/doom/init.el" ]; then
         mkdir -p "$XDG_CONFIG_HOME/doom"
         cd "$XDG_CONFIG_HOME/doom"
         ${pkgs.git}/bin/git init 
         ${pkgs.git}/bin/git remote add origin "${configRepoUrl}"
         ${pkgs.git}/bin/git fetch
         ${pkgs.git}/bin/git checkout origin/master -ft
         cd "$XDG_CONFIG_HOME"/emacs/bin/
         ./doom install 
      fi
      if [ ! -d "~/org" ]; then
        mkdir -p "~/org"
      fi
    '';
  };
}



