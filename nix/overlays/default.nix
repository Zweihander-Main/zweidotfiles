# This file defines overlays
{inputs, ...}: {
  # This one brings our custom packages from the 'pkgs' directory
  additions = final: _prev: import ../pkgs {pkgs = final;};

  # https://nixos.wiki/wiki/Overlays
  modifications = final: prev: {
    dmenu = prev.dmenu.overrideAttrs (old: {
      src = prev.fetchFromGitHub {
        owner = "Zweihander-Main";
        repo = "zwei_dmenu";
        rev = "fbfc73dcb444fc1cca47efa830f5f3e5c86d39c8";
        hash = "sha256-pIRDKjh8XBDiJoNfOlISEjxHUOSV8B37HCi3qUR7bUs=";
      };
    });
    dwm = prev.dwm.overrideAttrs (old: {
        src = prev.fetchFromGitHub {
          owner = "Zweihander-Main";
          repo = "zwei_dwm";
          rev = "b873e4bba2b44eedfbd3b01658cc9e23f349d304";
          hash = "sha256-ePmGMT3OpvPIkJJZqT/0o70PtKUiixdrQxIHeL80whc=";
        };
    });
  };

  # When applied, the unstable nixpkgs set (declared in the flake inputs) will
  # be accessible through 'pkgs.unstable'
  unstable-packages = final: _prev: {
    unstable = import inputs.nixpkgs-unstable {
      system = final.system;
      config.allowUnfree = true;
    };
  };
}

