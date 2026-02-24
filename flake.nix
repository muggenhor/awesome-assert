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

      clang-tools ? null,

      lib,
    }: stdenv.mkDerivation {
      pname = "awesome-assert";
      inherit version;

      src = ./.;

      nativeBuildInputs = [
        cmake
        ninja
      ] ++ lib.optional (clang-tools != null) clang-tools;

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
      build-envs = {
        clang = rec {
          inherit pkgs;
          stdenv = pkgs.clangStdenv;
          callPackage = pkg: overrides: pkgs.callPackage pkg ({ inherit stdenv; } // overrides);
        };
        gcc = rec {
          inherit pkgs;
          stdenv = pkgs.gccStdenv;
          callPackage = pkg: overrides: pkgs.callPackage pkg ({ inherit stdenv; } // overrides);
        };
      };
    in builtins.listToAttrs (
      builtins.concatMap (name: let
        build-env = build-envs.${name};
      in map (cxxstd:
      {
        name = "cpp${toString cxxstd}-${name}";
        value = build-env.callPackage awesome-assert' {
          cmakeFlags = [ "-DCMAKE_CXX_STANDARD=${toString cxxstd}" ] ++
            # Because clang-tidy can't handle GCC's C++20 module parameters that CMake automatically adds
            pkgs.lib.optional (cxxstd >= 20 && build-env.stdenv.cc.isGNU) "-DCLANG_TIDY_EXE=NO"
          ;
        };
      }) [ 17 20 23 ])
      (builtins.attrNames build-envs)
    ) // {
      cppcheck = pkgs.runCommand "cppcheck.log" rec {
        cppcheck = pkgs.lib.getExe' pkgs.cppcheck "cppcheck";
        pkg = (pkgs.callPackage awesome-assert' {}).out;
        inherit (pkg) src;
      } ''
        "$cppcheck" --template="{file}:{line}: {severity} ({id}): {message}" \
          --enable=style --force --std=c++17 -j 8 \
          -U__FUNCSIG__ -U_MSC_VER \
          -U__GNUC__ -U__clang__ -U__GLIBCXX__ -U_LIBCPP_VERSION \
          -I"$pkg/include" \
          "$src/src" 2> "$out"
        if [ -s "$out" ]; then
          cat "$out" >&2
          exit 1
        fi
      '';
    };
  });
}
