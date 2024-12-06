{config, ...}: {
  config = {
    hostAttr = {
      preinstalled = {
        systemd = false;
        udiskie = false;
        alacritty = false;
        emacs = false;
        vim = false;
      };
    };
  };
}
