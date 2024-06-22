{
  inputs,
  lib,
  config,
  pkgs,
  outputs,
  secrets,
  ...
}: {
  imports = [
    ./default.nix
    ./alacritty
    ./chezmoi
    ./chromium
    ./copyq
    ./coreutils
    ./dunst
    ./emacs
    ./flameshot
    ./lf
    ./mouseless
    ./redshift
    ./shellscripts
    ./stalonetray
    ./sxhkd
    ./syncthing
    ./time
    ./tmux
    ./udiskie
    ./x11
  ];

  home.packages = with pkgs; [
    # user programs
    firefox
    zathura
    # dev - c/cpp
    # gcc
    clang
    clang-tools
    cmake
    codespell
    conan
    cppcheck
    doxygen
    gdb
    gtest
    lcov
    vcpkg
    # dev - python
    python3
  ];
}
