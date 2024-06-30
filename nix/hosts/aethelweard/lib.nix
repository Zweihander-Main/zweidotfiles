{config, ...}: {
  config = {
    hostAttr = {
      monitor = {
        ddcci = true;
        nightTemp = 1000;
      };
      preinstalled = {
        systemd = true;
        udiskie = false;
        alacritty = false;
        emacs = false;
        vim = false;
      };
      paths = {
        systemdUserPkgServiceFiles = "/etc/systemd/user/";
      };
    };
  };
}
