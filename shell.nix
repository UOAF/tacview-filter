let project = import ./default.nix;
in project.shellFor {
    exactDeps = true;
    tools = {
      cabal = "latest";
      hlint = "latest";
      # Currently torpedoed by some build error with ghcide on 9.8
      #haskell-language-server = "latest";
    };
}
