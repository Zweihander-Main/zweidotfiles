{pkgs ? import <nixpkgs> {}, ...}: {
  iosevka-ss09 = pkgs.callPackage ./iosevka-ss09.nix {};
  iosevka-term-ss09 = pkgs.callPackage ./iosevka-term-ss09.nix {};
  iosevka-aile = pkgs.callPackage ./iosevka-aile.nix {};
  mcfly-fzf = pkgs.callPackage ./mcfly-fzf.nix {};
  xmouseless = pkgs.callPackage ./xmouseless.nix {};
}
