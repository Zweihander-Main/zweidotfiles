{ pkgs ? import <nixpkgs> { }, ... }: {
  mcfly-fzf = pkgs.callPackage ./mcfly-fzf.nix { };
}
