{ pkgs, config, lib, secrets, ... }:
let homeDir = config.home.homeDirectory;
in {
  home.packages = with pkgs; [
    clang
    clang-tools
    cmake
    codespell
    # conan # can't get it to build
    cppcheck
    doxygen
    gdb
    gtest
    lcov
    vcpkg
  ];
}

