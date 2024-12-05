{ pkgs, config, lib, ... }:
let homeDir = config.home.homeDirectory;
in {
  home.packages = with pkgs; [ pipe-viewer mplayer ];

  systemd.user.tmpfiles.rules = [ "d ${homeDir}/vids/towatch" ];

  xdg.configFile."pipe-viewer/pipe-viewer.conf".source =
    lib.mine.mkTemplate ./pipe-viewer.conf.j2 {
      player = config.hostAttr.programs.pipeViewer.player;
    };
}
