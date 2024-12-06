{config, ...}: {
  config = {
    hostAttr = {
      type = "play";
      monitor = {
        ddcci = false;
        nightTemp = 1200;
      };
      preinstalled = {
        systemd = true;
        udiskie = true;
        alacritty = true;
        emacs = true;
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
