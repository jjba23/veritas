# veritas
#
# SPDX-License-Identifier: MIT
# Copyright © Josep Bigorra <jjbigorra@gmail.com>

# The package of this checkout as it stands.  It is the same derivation as the
# `guile-veritas' package in nixpkgs -- same phases, same layout under $out --
# so that a change which breaks this build breaks that one for the same reason.
# What differs is the source: this file builds the working tree, while nixpkgs
# pins a released tag and its hash.  Keep `version' in step with the tag you
# release, because nothing here can read it from the library.

{ lib
, stdenv
, guile
, guile-fibers
, guile-json
}:

stdenv.mkDerivation {
  pname = "guile-veritas";
  version = "1.3.1";

  src = ./.;

  nativeBuildInputs = [ guile ];

  buildInputs = [ guile ];

  propagatedBuildInputs = [
    guile-fibers
    guile-json
  ];

  buildPhase = ''
    runHook preBuild

    siteDir=$out/${guile.siteDir}
    install -d "$siteDir"
    cp -R src/. "$siteDir"/

    export GUILE_AUTO_COMPILE=0
    export GUILE_LOAD_PATH="$siteDir''${GUILE_LOAD_PATH:+:$GUILE_LOAD_PATH}"

    find src -name '*.scm' | while IFS= read -r file; do
      rel="''${file#src/}"
      guild compile -o "$siteDir/''${rel%.scm}.go" "$file"
    done

    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall
    runHook postInstall
  '';

  meta = with lib; {
    description = "Testing framework for GNU Guile";
    homepage = "https://github.com/jjba23/veritas";
    license = licenses.mit;
    platforms = guile.meta.platforms;
  };
}
