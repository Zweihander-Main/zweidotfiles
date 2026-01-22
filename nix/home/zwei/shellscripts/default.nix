{ pkgs, config, lib, secrets, ... }:
let homeDir = config.home.homeDirectory;
in {
  home.packages = with pkgs; [ git ];

  home.activation = {
    bootstrapShellScripts =
      lib.hm.dag.entryAfter [ "writeBoundary" "installPackages" "git" ] ''
        if ! [ -d "${homeDir}/dev/sys/shell_scripts" ]; then
          mkdir -p "${homeDir}/dev/sys"
          ${pkgs.git}/bin/git clone https://github.com/Zweihander-Main/shell_scripts "${homeDir}/dev/sys/shell_scripts"
        else
          cd "${homeDir}/dev/sys/shell_scripts"
          ${pkgs.git}/bin/git pull origin master
        fi
      '';
  };

  home.file.".local/bin/check_i3lock".source =
    config.lib.file.mkOutOfStoreSymlink
    "${homeDir}/dev/sys/shell_scripts/start_i3lock/check_i3lock.sh";
  home.file.".local/bin/delete_old_files".source =
    config.lib.file.mkOutOfStoreSymlink
    "${homeDir}/dev/sys/shell_scripts/delete_old_files/delete_old_files.sh";
  home.file.".local/bin/download_nextcloud".source =
    config.lib.file.mkOutOfStoreSymlink
    "${homeDir}/dev/sys/shell_scripts/download_nextcloud/download_nextcloud.sh";
  home.file.".local/bin/log_dunst".source = config.lib.file.mkOutOfStoreSymlink
    "${homeDir}/dev/sys/shell_scripts/log_dunst/log_dunst.sh";
  home.file.".local/bin/memo_to_inbox".source =
    config.lib.file.mkOutOfStoreSymlink
    "${homeDir}/dev/sys/shell_scripts/memo_to_inbox/memo_to_inbox.sh";
  home.file.".local/bin/open_video".source = config.lib.file.mkOutOfStoreSymlink
    "${homeDir}/dev/sys/shell_scripts/open_video/open_video.sh";
  home.file.".local/bin/openbb".source = config.lib.file.mkOutOfStoreSymlink
    "${homeDir}/dev/sys/shell_scripts/openbb/openbb.sh";
  home.file.".local/bin/start_dwm".source = config.lib.file.mkOutOfStoreSymlink
    "${homeDir}/dev/sys/shell_scripts/start_dwm/start_dwm.sh";
  home.file.".local/bin/start_gpg".source = config.lib.file.mkOutOfStoreSymlink
    "${homeDir}/dev/sys/shell_scripts/start_gpg/start_gpg.sh";
  home.file.".local/bin/start_i3lock".source =
    config.lib.file.mkOutOfStoreSymlink
    "${homeDir}/dev/sys/shell_scripts/start_i3lock/start_i3lock.sh";
  home.file.".local/bin/start_keepassxc".source =
    config.lib.file.mkOutOfStoreSymlink
    "${homeDir}/dev/sys/shell_scripts/start_keepassxc/start_keepassxc.sh";
  home.file.".local/bin/start_waydroid".source =
    config.lib.file.mkOutOfStoreSymlink
    "${homeDir}/dev/sys/shell_scripts/start_waydroid/start_waydroid.sh";
  home.file.".local/bin/toggle_sink".source =
    config.lib.file.mkOutOfStoreSymlink
    "${homeDir}/dev/sys/shell_scripts/toggle_sink/toggle_sink.sh";
  home.file.".local/bin/wait-for-keepassxc-requirements".source =
    config.lib.file.mkOutOfStoreSymlink
    "${homeDir}/dev/sys/shell_scripts/start_keepassxc/wait-for-keepassxc-requirements.sh";
}
