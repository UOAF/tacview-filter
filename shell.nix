let project = import ./default.nix;
in project.shellFor {
    exactDeps = true;
    tools = {
      cabal = "latest";
      hlint = "latest";
      haskell-language-server = "latest";
    };
    withHoogle = true;
}
