{lib, pkgs, ...} @ args:
with lib; let
  _lib = self: let
    callLibs = file: import file ({lib = self; pkgs = pkgs;} // args);
  in {
    templating = callLibs ./templating.nix;
  };
  # NOTE `makeExtensible` allows `self` referencing
  mine = makeExtensible _lib;
in
  mine.extend (self: super:
    foldr (a: b: a // b) {} (attrValues super))
