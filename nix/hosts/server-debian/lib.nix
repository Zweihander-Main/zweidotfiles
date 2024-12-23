{config, ...}: {
  config = {
    hostAttr = {
      preinstalled = {
        alacritty = false;
        emacs = false;
        pipeViewer = false;
        systemd = true;
        udiskie = false;
        vim = false;
      };
      paths = {
        systemdUserPkgServiceFiles = "/usr/lib/systemd/user/";
      };
    };
  };
}
