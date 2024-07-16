{lib, pkgs, ...}: let
  # Jinja2 templating
  mkTemplate = src: params:
    pkgs.runCommand "template-${src}" {
      buildInputs = [pkgs.j2cli];
      passAsFile = [
        "paramsJson"
      ];
      paramsJson = builtins.toJSON params;
    }
    ''
      ${pkgs.j2cli}/bin/j2 -f json ${src} "$paramsJsonPath" > "$out"
    '';
in {
  inherit mkTemplate;
}
