{
  lib,
  stdenvNoCC,
  fetchzip,
}:
stdenvNoCC.mkDerivation rec {
  pname = "iosevka-aile";
  version = "30.3.3";

  src = fetchzip {
    url = "https://github.com/be5invis/Iosevka/releases/download/v${version}/PkgTTF-IosevkaAile-${version}.zip";
    stripRoot = false;
    hash = "sha256-DT/44+zE1bVlimtzh1ObVW9lE+JdkPtjIGW1vJdU1HA=";
  };

  installPhase = ''
    runHook preInstall

    install -Dm644 *.ttf -t $out/share/fonts/TTF

    runHook postInstall
  '';

  meta = {
    description = "Typeface family designed for coding, terminal use and technical documents -- Aile variant.";
    platforms = with lib.platforms; all;
    homepage = "https://github.com/be5invis/Iosevka";
    license = lib.licenses.ofl;
  };
}
