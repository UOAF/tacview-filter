let haskellSrc = builtins.fetchTarball "https://github.com/input-output-hk/haskell.nix/archive/6a8eaba643320340ca56648c055148d1d4c64e1c.tar.gz";
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
          # Some fuckery with Haskell.nix - not recognizing optional build flags?
          packages.unix.flags = { "os-string" = true; };
          packages.directory.flags = { "os-string" = true; };
        }

      ];
    };
in project.shellFor {
    exactDeps = true;
    tools = {
      cabal = "latest";
      # not linking today
      #hlint = "latest";
      haskell-language-server = "latest";
    };
}
