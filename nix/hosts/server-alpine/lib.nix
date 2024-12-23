{config, ...}: {
  config = {
    hostAttr = {
      preinstalled = {
        alacritty = false;
        emacs = false;
        pipeViewer = false;
        systemd = false;
        udiskie = false;
        vim = false;
      };
    };
  };
}
