{ mkDerivation, aeson, base, base64-bytestring, bcrypt, bloodhound
, bytestring, containers, directory, dotenv, fast-logger, filepath
, hpack, hslogger, http-client, http-types, imagemagick
, monad-control, monad-logger, mtl, optparse-applicative
, parser-combinators, persistent, persistent-postgresql
, persistent-template, random, safe, servant, servant-auth
, servant-multipart, servant-server, stdenv, text, time
, transformers, unordered-containers, utf8-string, uuid, wai
, wai-cors, wai-extra, warp
}:
mkDerivation {
  pname = "donnabot-service";
  version = "0.0.12.3";
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
}
