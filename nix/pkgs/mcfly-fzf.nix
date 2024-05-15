{ stdenv, fetchurl }:

stdenv.mkDerivation rec {
  pname = "mcfly-fzf";
  version = "0.1.2";

  src = fetchurl {
    url = "https://github.com/bnprks/mcfly-fzf/releases/download/0.1.2/mcfly-fzf-0.1.2-x86_64-unknown-linux-musl.tar.gz";
    sha256 = "25ca2b6545921ac08ad60d82ce3873050e7eae4887497e28b303e0b0d65150b1";  
  };

  sourceRoot = ".";

  unpackCmd = "tar -xzf $src";

  installPhase = ''
    mkdir -p $out/bin
    cp mcfly-fzf $out/bin
  '';
}
