{
  pkgs,
  config,
  lib,
  secrets,
  ...
}: {
  home.packages = with pkgs; [
    ungoogled-chromium
    profile-sync-daemon
  ];

  # For profile
  services.psd = {
    enable = true;
    resyncTimer = "1h";
  };

  # For cache
  systemd.user.tmpfiles.rules = ["d /run/user/1000/chromium-cache"];
  home.file.".cache/chromium".source = config.lib.file.mkOutOfStoreSymlink "/run/user/1000/chromium-cache";
}
