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

    awesome-assert' = {
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
    };

  in {
    packages = rec {
      awesome-assert = pkgs.callPackage awesome-assert' {};
      default = awesome-assert;
    };

    checks = let
      # FIXME: use older Nix version to test with GCC 5 and 7 respectively
      build-envs = {
        clang = rec {
          inherit pkgs;
          stdenv = pkgs.clang12Stdenv;
          callPackage = pkg: overrides: pkgs.callPackage pkg ({ inherit stdenv; } // overrides);
        };
        gcc = rec {
          inherit pkgs;
          stdenv = pkgs.gcc9Stdenv;
          callPackage = pkg: overrides: pkgs.callPackage pkg ({ inherit stdenv; } // overrides);
        };
      };
    in builtins.listToAttrs (
      builtins.concatMap (name: let
        build-env = build-envs.${name};
      in map (cxxstd:
      {
        name = "cpp${cxxstd}-${name}";
        value = build-env.callPackage awesome-assert' { cmakeFlags = [ "-DCMAKE_CXX_STANDARD=${cxxstd}" ]; };
      }) [ "14" "17" ])
      (builtins.attrNames build-envs)
    );
  });
}
