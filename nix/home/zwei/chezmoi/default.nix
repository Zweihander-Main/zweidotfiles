{
  pkgs,
  config,
  lib,
  secrets,
  ...
}: let
  homeDir = config.home.homeDirectory;
in {
  home.packages = with pkgs; [
    git
    chezmoi
  ];

  xdg.configFile."chezmoi/chezmoi.json".text = ''
    {
        "data": {
            "enable_crypt": "false",
            "org_directory": "${homeDir}/org",
            "osid": "linux-nixos"
        },
        "encryption": "gpg",
        "gpg": {
            "recipient": "${secrets.gpg.recipient}"
        }
    }
  '';
  home.activation = {
    initChezmoi = lib.hm.dag.entryAfter ["writeBoundary" "installPackages" "git" "chezmoi" "xdg"] ''
      mkdir -p "$XDG_DATA_HOME/chezmoi"
      cd "$XDG_DATA_HOME/chezmoi"
      if ! [ -d ".git" ]; then
        ${pkgs.git}/bin/git init
        ${pkgs.git}/bin/git remote add origin https://github.com/Zweihander-Main/zweidotfiles
        ${pkgs.git}/bin/git remote add gorigin git://github.com/Zweihander-Main/zweidotfiles.git
      fi
      ${pkgs.git}/bin/git pull --set-upstream origin master
      ${pkgs.chezmoi}/bin/chezmoi apply || true
    '';
  };
}
