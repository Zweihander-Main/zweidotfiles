{pkgs, ...}: {
  home.packages = with pkgs; [
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
    (aspellWithDicts (ds: with ds; [en en-computers en-science]))
    # :tools lookup & :lang org +roam
    sqlite
    # :tools ansible
    ansible
    # :tools docker
    dockfmt
    # :tools editoconfig
    editorconfig-core-c
  ];

  emacs = {
    enable = true;
    doom.enable = true;
  };
}
