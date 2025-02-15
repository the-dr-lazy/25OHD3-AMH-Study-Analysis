{
  pkgs,
  ...
}:

pkgs.mkShell {
  buildInputs = [
    pkgs.bashInteractive
  ];

  nativeBuildInputs = builtins.concatMap builtins.attrValues [
    ###################################################
    # Languages:
    {
      rstudio = pkgs.rstudioWrapper.override {
        packages = builtins.attrValues {
          inherit (pkgs.rPackages)
            tidyverse
            ggpubr
            pastecs
            xlsx
            ggstatsplot
            psych
            DescTools
            systemfonts
            hrbrthemes
            gginference
            apa
            rstatix
            ;
        };
      };
    }
  ];
}
