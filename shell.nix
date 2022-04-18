{ pure ? false, source-repo-override ? { } }:
let
  packages = import ./. { inherit source-repo-override; };
  cardano-node = import
    (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/cardano-node";
      # A standard release compatible with the cardano-wallet commit above is always preferred.
      rev = "1.34.1";
      sha256 = "1hh53whcj5y9kw4qpkiza7rmkniz18r493vv4dzl1a8r5fy3b2bv";
    })
    { };
  inherit (packages) pkgs p2e;
  inherit (p2e) haskell;

in
  haskell.project.shellFor {
    withHoogle = false;

    nativeBuildInputs = with p2e; [
      hlint
      cabal-install
      haskell-language-server
      stylish-haskell
      pkgs.niv
      cardano-repo-tool
      pkgs.ghcid
      cardano-node.cardano-node
      cardano-node.cardano-cli
      # HACK: This shouldn't need to be here.
      pkgs.lzma.dev
    ] ++ (pkgs.lib.optionals pure [
      pkgs.git
      pkgs.cacert
      pkgs.curl
      pkgs.jq
    ]);
  }
