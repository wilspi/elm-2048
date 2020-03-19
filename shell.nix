let
  # Update nixpkgs from release: https://github.com/NixOS/nixpkgs/releases/tag/20.03-beta
  nixpkgs = import (builtins.fetchTarball https://github.com/NixOS/nixpkgs/archive/20.03-beta.tar.gz) {
    overlays = [];
    config = {};
  };
  frameworks = nixpkgs.darwin.apple_sdk.frameworks;

in
  with nixpkgs;

  stdenv.mkDerivation {
    name = "elm-2048";

    nativeBuildInputs = [
      elmPackages.elm-format
      elmPackages.elm

      file
      zsh
      wget
      locale
      which
      vim
      less
      htop
      curl
      man
      git
      gitAndTools.diff-so-fancy
      openssl
      pkgconfig
      perl
      nixpkgs-fmt
      cacert
    ] ++ (
      stdenv.lib.optionals stdenv.isDarwin [
        frameworks.Security
        frameworks.CoreServices
        frameworks.CoreFoundation
        frameworks.Foundation
      ]
    );

    # ENV Variables
    HISTFILE = "${toString ./.}/.zsh_history";
    SOURCE_DATE_EPOCH = 315532800;
    LIBCLANG_PATH = "${llvmPackages.libclang}/lib";

    # Post Shell Hook
    shellHook = (
      if !pkgs.stdenv.isDarwin then
        ""
      else ''
        export NIX_LDFLAGS="-F${frameworks.CoreFoundation}/Library/Frameworks -framework CoreFoundation $NIX_LDFLAGS";
      ''
    ) + ''
      echo "Using ${elmPackages.elm.name}"
      echo "env: elm-2048 activated";
    '';
  }
