{config, ...}: {
  config = {
    hostAttr = {
      preinstalled = {
        systemd = true;
        udiskie = false;
      };
    };
  };
}