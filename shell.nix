let
  old = import <nixpkgs> {};
  ppFixed = old.fetchFromGitHub {
    owner = "chreekat";
    repo = "show-prettyprint";
    rev = "list-fixes";
    sha256 = "0fxp286wqx6cvqd4x88421p6jpdxs0szdfv0mvzqpz7ikim33d3q";
  };
  h = old.haskellPackages.extend (self: super: {
    scalpel-core = self.callHackageDirect {
      pkg = "scalpel-core";
      ver = "0.6.0";
      sha256 = "12fy859pwxr42pn9fr3cdbkyjzw2i92vchp2rs6m50f9m8qspa1r";
    } {};
    #show-prettyprint = self.callPackage ../../src/show-prettyprint {};
    show-prettyprint = self.callCabal2nix "show-prettyprint" ppFixed {};
  });
in
old.mkShell {
  buildInputs = [
    (h.ghcWithPackages (p: [
      p.ghcid
      p.scalpel-core
      p.doctest
      p.show-prettyprint
    ]))
  ];
}
