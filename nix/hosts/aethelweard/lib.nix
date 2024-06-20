{config, ...}: {
  config = {
    hostAttr = {
      monitor = {
        ddcci = true;
        nightTemp = 1000;
      };
      preinstalled = {
        systemd = true;
        udiskie = false;
      };
    };
  };
}
