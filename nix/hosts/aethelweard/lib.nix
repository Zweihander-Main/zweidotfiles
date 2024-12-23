{config, ...}: {
  config = {
    hostAttr = {
      monitor = {
        ddcci = true;
        nightTemp = 1000;
        pixelRatio = 2;
      };
      preinstalled = {
        alacritty = false;
        emacs = false;
        pipeViewer = false;
        systemd = true;
        udiskie = false;
        vim = false;
      };
      programs = {
        pipeViewer = {
          player = "audioonly";
        };
      };
      paths = {
        systemdUserPkgServiceFiles = "/etc/systemd/user/";
      };
    };
  };
}
