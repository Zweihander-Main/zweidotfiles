{pkgs ? import <nixpkgs> {}, ...}: {
  mcfly-fzf = pkgs.callPackage ./mcfly-fzf.nix {};
  xmouseless = pkgs.callPackage ./xmouseless.nix {};
}
