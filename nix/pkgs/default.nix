{pkgs ? import <nixpkgs> {}, ...}: {
  iosevka-ss09 = pkgs.callPackage ./iosevka-ss09.nix {};
  mcfly-fzf = pkgs.callPackage ./mcfly-fzf.nix {};
  xmouseless = pkgs.callPackage ./xmouseless.nix {};
}
