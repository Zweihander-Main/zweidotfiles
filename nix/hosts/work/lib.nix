{config, ...}: {
  config = {
    hostAttr = {
      type = "work";
      monitor = {
        ddcci = false;
        nightTemp = 1200;
      };
      preinstalled = {
        alacritty = true;
        emacs = true;
        pipeViewer = true;
        systemd = true;
        udiskie = true;
        vim = true;
      };
      programs = {
        pipeViewer = {
          player = "mpv";
        };
      };
      paths = {
        systemdUserPkgServiceFiles = "/usr/lib/systemd/user/";
      };
    };
  };
}
