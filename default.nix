let haskellSrc = builtins.fetchTarball "https://github.com/input-output-hk/haskell.nix/archive/7448cb3337456fbd8208b1c8f1db6b33654e03c0.tar.gz";
    haskellNix = import haskellSrc {};
    # Import nixpkgs and pass the haskell.nix provided nixpkgsArgs
    pkgs = import
        # haskell.nix provides access to the nixpkgs pins which are used by our CI,
        # hence you will be more likely to get cache hits when using these.
        # But you can also just use your own, e.g. '<nixpkgs>'.
        haskellNix.sources.nixpkgs
        # These arguments passed to nixpkgs, include some patches and also
        # the haskell.nix functionality itself as an overlay.
        haskellNix.nixpkgsArgs;
in pkgs.haskell-nix.project {
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "bms-tacview-filter";
    src = ./.;
  };
  modules = [
    { enableProfiling = true;
      enableLibraryProfiling = true;
    }
  ];
}
