{lib, ...} @ args:
with lib; let
  _lib = self: let
    callLibs = file: import file ({lib = self;} // args);
  in {
    mkTemplate = callLibs ./mkTemplate.nix;
  };
  # NOTE `makeExtensible` allows `self` referencing
  mine = makeExtensible _lib;
in
  mine.extend (self: super:
    foldr (a: b: a // b) {} (attrValues super))
