{config, ...}: {
  config = {
    hostAttr = {
      preinstalled = {
        systemd = false;
        udiskie = false;
      };
    };
  };
}
