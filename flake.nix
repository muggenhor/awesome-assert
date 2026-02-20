# SPDX-License-Identifier: LGPL-3.0-or-later

{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = inputs@{ self, flake-utils, nixpkgs, ... }:
  flake-utils.lib.eachDefaultSystem (system:
  let
    versionOf = flake:
      with nixpkgs.lib;
      "${if flake ? lastModifiedDate then (substring 0 8 flake.lastModifiedDate) else "19700101"}${
        optionalString (flake ? revCount) "-${toString flake.revCount}"
      }-${
        if (flake ? shortRev) || (flake ? dirtyShortRev) then "g${flake.shortRev or flake.dirtyShortRev}" else "dirty"
      }";

    pkgs = import nixpkgs {
      inherit system;
    };

    awesome-assert = pkgs.callPackage ({
      version ? versionOf self,
      toolchain ? [],
      cmakeFlags ? [],

      stdenv,
      cmake,
      ninja,
    }: stdenv.mkDerivation {
      pname = "awesome-assert";
      inherit version;

      src = ./.;

      nativeBuildInputs = [
        cmake
        ninja
      ];

      cmakeFlags = [
      ] ++ toolchain ++ cmakeFlags;

      preConfigure = ''
        cmakeFlagsArray+=("-DCMAKE_INSTALL_ARCHIVEDIR=''${!outputDev}/lib")
      '';

      ${if self ? lastModified then "SOURCE_DATE_EPOCH" else null} = self.lastModified;

      outputs = [ "lib" "out" ];

      cmakeBuildType = "RelWithDebInfo";
      doCheck = true;
    }) {};

  in {
    packages = rec {
      inherit awesome-assert;
      default = awesome-assert;
    };
  });
}
