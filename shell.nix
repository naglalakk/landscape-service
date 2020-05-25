{ nixpkgs ? import ./packages.nix {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, base64-bytestring, bcrypt
      , bloodhound, bytestring, containers, directory, dotenv
      , fast-logger, filepath, hpack, hslogger, http-client, http-types
      , imagemagick, monad-control, monad-logger, mtl
      , optparse-applicative, parser-combinators, persistent
      , persistent-postgresql, persistent-template, random, safe, servant
      , servant-auth, servant-multipart, servant-server, stdenv, text
      , time, transformers, unordered-containers, utf8-string, uuid, wai
      , wai-cors, wai-extra, warp
      }:
      mkDerivation {
        pname = "donnabot-service";
        version = "0.0.12.4";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          aeson base bcrypt bloodhound bytestring containers directory dotenv
          fast-logger filepath hslogger http-client http-types imagemagick
          monad-control monad-logger mtl parser-combinators persistent
          persistent-postgresql persistent-template random safe servant
          servant-auth servant-multipart servant-server text time
          transformers unordered-containers uuid wai wai-cors wai-extra warp
        ];
        libraryToolDepends = [ hpack ];
        executableHaskellDepends = [
          base base64-bytestring bytestring dotenv fast-logger hslogger
          http-types monad-logger mtl optparse-applicative persistent
          persistent-postgresql persistent-template random safe text time
          utf8-string wai wai-cors wai-extra warp
        ];
        prePatch = "hpack";
        description = "donnabot.dev service";
        license = stdenv.lib.licenses.mit;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
