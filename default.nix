{}:
(import ./reflex-platform {}).project ({ pkgs, ... }: {
  packages = {
    common = ./common;
    backend = ./backend;
    frontend = ./frontend;
    sforce-migration = ../sforce-migration;
  };
  

  shells = {
    ghc = ["common" "backend" "frontend" "sforce-migration"];
    ghcjs = ["common" "frontend"];
  };

  overrides = self: super: rec {
        optparse-applicative = self.callCabal2nix "optparse-applicative" (pkgs.fetchFromGitHub {
          owner = "pcapriotti";
          repo = "optparse-applicative";
          rev = "72ae4b69875e1702de36f083b32b106f6da6926e";
          sha256 = "1b19wjgsnlr5399qp0qhk2w1bqyzvabkkxr2iw3jkfx4f6zb2lp0";
        }) {};
    };
})
