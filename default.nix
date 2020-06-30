{ pkgs ? import ./packages.nix {}, mkDerivation, aeson, base, base64-bytestring, bcrypt, bloodhound
, bytestring, containers, directory, dotenv, fast-logger, filepath
, hedis, hpack, hslogger, http-client, http-types, imagemagick
, monad-control, monad-logger, mtl, optparse-applicative
, parser-combinators, persistent, persistent-postgresql
, persistent-template, random, safe, servant, servant-auth
, servant-multipart, servant-server, stdenv, text, time
, transformers, unordered-containers, utf8-string, uuid, wai
, wai-cors, wai-extra, warp
}:
let 
  gitignoreSrc = pkgs.fetchFromGitHub { 
    owner = "hercules-ci";
    repo = "gitignore.nix";
    rev = "7415c4feb127845553943a3856cbc5cb967ee5e0";
    sha256 = "sha256:1zd1ylgkndbb5szji32ivfhwh04mr1sbgrnvbrqpmfb67g2g3r9i";
  };
  inherit (import gitignoreSrc { inherit (pkgs) lib; }) gitignoreSource;
in 
  mkDerivation {
    pname = "donnabot-service";
    version = "0.0.13.10";
    src = gitignoreSource ./.;
    isLibrary = true;
    isExecutable = true;
    doHaddock = false;
    libraryHaskellDepends = [
      aeson base base64-bytestring bcrypt bloodhound bytestring
      containers directory dotenv fast-logger filepath hedis hslogger
      http-client http-types imagemagick monad-control monad-logger mtl
      optparse-applicative parser-combinators persistent
      persistent-postgresql persistent-template random safe servant
      servant-auth servant-multipart servant-server text time
      transformers unordered-containers utf8-string uuid wai wai-cors
      wai-extra warp
    ];
    libraryToolDepends = [ hpack ];
    executableHaskellDepends = [
      base base64-bytestring bloodhound bytestring dotenv fast-logger
      hslogger http-types monad-logger mtl optparse-applicative
      persistent persistent-postgresql persistent-template random safe
      text time utf8-string wai wai-cors wai-extra warp
    ];
    prePatch = "hpack";
    description = "donnabot.dev service";
    license = stdenv.lib.licenses.mit;
  }
