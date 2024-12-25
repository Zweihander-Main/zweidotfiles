{
  pkgs,
  lib,
  ...
}: {
  home.packages = with pkgs; [syncthing];

  services.syncthing = {
    enable = true;
    tray.enable = true;
  };

  systemd.user.services.syncthing = {
    Install = lib.mkForce {WantedBy = ["wm.target"];};
  };
}
