let haskellSrc = builtins.fetchTarball "https://github.com/input-output-hk/haskell.nix/archive/7a6dc9d12ff18c6d96e0f63a7ebf1610aa274d46.tar.gz";
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
    project = pkgs.haskell-nix.stackProject {
      src = pkgs.haskell-nix.cleanSourceHaskell {
        name = "tacview";
        src = ./.;
      };
      modules = [
        { enableProfiling = true;
          enableLibraryProfiling = true;
        }
      ];
    };
in project.shellFor {
    exactDeps = true;
    tools = {
      cabal = "latest";
      hlint = "latest";
      # Currently torpedoed by some build error with ghcide on 9.8
      # haskell-language-server = "2.6.0.0";
      # haskell-language-server = "latest";
    };
}
