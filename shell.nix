let
  old = import <nixpkgs> {};
  h = old.haskellPackages.extend (self: super: {
    scalpel-core = self.callHackageDirect {
      pkg = "scalpel-core";
      ver = "0.6.0";
      sha256 = "12fy859pwxr42pn9fr3cdbkyjzw2i92vchp2rs6m50f9m8qspa1r";
    } {};
    show-prettyprint = self.callHackageDirect {
      pkg = "show-prettyprint";
      ver = "0.3";
      sha256 = "114h5f3lm473vv34nawp53ci580qq31fgbfgg9wal68d11ibl715";
    } {};
  });
in
old.mkShell {
  buildInputs = [
    (h.ghcWithPackages (p: [
      p.ghcid
      p.scalpel-core
      p.servant
      p.show-prettyprint
      p.template-haskell
    ]))
  ];
}
