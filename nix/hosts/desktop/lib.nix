{config, ...}: {
  config = {
    hostAttr = {
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
    };
  };
}
