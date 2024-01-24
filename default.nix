let haskellSrc = builtins.fetchTarball "https://github.com/input-output-hk/haskell.nix/archive/c9129a2eb14aff7c9db534023cb04f6ff6bfa152.tar.gz";
    haskellNix = import haskellSrc {};
    # Import nixpkgs and pass the haskell.nix provided nixpkgsArgs
    pkgs = import <nixos>
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
