{
  lib,
  stdenvNoCC,
  fetchzip,
}:
stdenvNoCC.mkDerivation rec {
  pname = "iosevka-term-ss09";
  version = "30.3.3";

  src = fetchzip {
    url = "https://github.com/be5invis/Iosevka/releases/download/v${version}/PkgTTF-IosevkaTermSS09-${version}.zip";
    stripRoot = false;
    hash = "sha256-gW4EnFKnItmPYyn+L/6/7NX9ChGWdfCHloOSdlzOeqA=";
  };

  installPhase = ''
    runHook preInstall

    install -Dm644 *.ttf -t $out/share/fonts/TTF

    runHook postInstall
  '';

  meta = {
    description = "Typeface family designed for coding, terminal use and technical documents -- SS09 Terminal variant.";
    platforms = with lib.platforms; all;
    homepage = "https://github.com/be5invis/Iosevka";
    license = lib.licenses.ofl;
  };
}
