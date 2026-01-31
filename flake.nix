  {
    inputs.flakelight-chez.url = "github:jdek/flakelight-chez";
    outputs = { flakelight-chez, ... }:
      flakelight-chez ./. {
        license = "WTFPL";
        devShell.shellHook = ''
          export CHEZSCHEMELIBDIRS="$CHEZSCHEMELIBDIRS:$PWD/src"
        '';
      };
  }
