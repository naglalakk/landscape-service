{}:
  let
    nixpkgs = import <nixpkgs> { inherit config; };
    config = {
      allowBroken = true;
      packageOverrides = pkgs:
        let
          hl = pkgs.haskell.lib;
          t = pkgs.lib.trivial;
          dontAndDisable = (t.flip t.pipe)
            [hl.dontCheck
             hl.dontCoverage
             hl.dontHaddock
            ];
        in rec {
          haskellPackages = pkgs.haskellPackages.override {
            overrides = self: super: {
              bloodhound = dontAndDisable 
                (super.callCabal2nix "bloodhound" (builtins.fetchGit {
                  url = "https://github.com/naglalakk/bloodhound";
                  rev = "4bd33e638b6ba1b9e580f1e9ab63821bacef36d6";
                }) {});
                imagemagick = 
                (pkgs.haskell.lib.appendConfigureFlags
                  (dontAndDisable
                    (super.callCabal2nix "imagemagick" (builtins.fetchGit {

                      url = "https://github.com/naglalakk/imagemagick";
                      rev = "07f1a54e3e1460b244f33e2082b4f126f1d6a832";

                    }) { 
                      imagemagick = pkgs.imagemagick; 
                    })
                  )
                  [ "--extra-include-dirs=${pkgs.imagemagick.dev}/include/ImageMagick"
                  ]
                );
            };
          };
        };
    };
  in nixpkgs
