{config, ...}: {
  config = {
    hostAttr = {
      preinstalled = {
        systemd = true;
        udiskie = false;
        alacritty = false;
        emacs = false;
        vim = false;
      };
      paths = {
        systemdUserPkgServiceFiles = "/usr/lib/systemd/user/";
      };
    };
  };
}
