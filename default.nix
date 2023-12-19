let haskellSrc = builtins.fetchTarball "https://github.com/input-output-hk/haskell.nix/archive/a9efc0ae3a607eaebc6e841ca3960134e9034077.tar.gz";
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
  compiler-nix-name = "ghc948";
}
