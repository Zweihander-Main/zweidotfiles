{ pkgs, ... }: {

  xdg.configFile."aspell/aspell.conf".source = ./aspell.conf;

  home.packages = with pkgs;
    [ (aspellWithDicts (ds: with ds; [ en en-computers en-science ])) ];
}
