{
  stdenv,
  fetchFromGitHub,
  libX11,
  libXtst,
  libXi,
}:
stdenv.mkDerivation rec {
  pname = "xmouseless";
  version = "2022.06.02";

  src = fetchFromGitHub {
    owner = "Zweihander-Main";
    repo = "xmouseless";
    rev = "db7f9490a5da418e5f56d7c5bea87e1dc31f2624";
    hash = "sha256-gYMZlVTyeZetFM/8AN0E4QPVffGXY37LiTL/m0mm1gI=";
  };

  buildInputs = [
    libX11
    libXtst
    libXi
  ];

  buildPhase = ''
    gcc -Wall -g -o xmouseless xmouseless.c -lX11 -lXtst -lpthread
  '';

  installPhase = ''
    mkdir -p $out/bin
    cp xmouseless $out/bin
  '';
}
