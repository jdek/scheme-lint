{
  description = "Pluggable linter for Chez Scheme with pattern matching";

  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-25.11";

  outputs = { self, nixpkgs }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      forAllSystems = nixpkgs.lib.genAttrs systems;
      version = builtins.readFile ./VERSION;
    in {
      packages = forAllSystems (system:
        let pkgs = nixpkgs.legacyPackages.${system};
        in {
          default = pkgs.stdenv.mkDerivation {
            pname = "scheme-lint";
            inherit version;
            src = self;

            buildInputs = [ pkgs.chez ];

            installPhase = ''
              mkdir -p $out/bin
              mkdir -p $out/share/scheme-lint/src
              mkdir -p $out/share/scheme-lint/rules

              # Install libraries
              cp -r src/scheme-lint $out/share/scheme-lint/src/

              # Install rules
              cp -r rules/standard $out/share/scheme-lint/rules/
              cp -r rules/examples $out/share/scheme-lint/rules/

              # Install CLI script
              cp bin/scheme-lint $out/share/scheme-lint/

              # Create wrapper script
              cat > $out/bin/scheme-lint <<EOF
#!/bin/sh
export CHEZSCHEMELIBDIRS="$out/share/scheme-lint/src:\$CHEZSCHEMELIBDIRS"
exec ${pkgs.chez}/bin/scheme --script $out/share/scheme-lint/scheme-lint "\$@"
EOF
              chmod +x $out/bin/scheme-lint
            '';

            meta = {
              description = "Pluggable linter for Chez Scheme with pattern matching";
              homepage = "https://github.com/eiri/scheme-lint";
              license = pkgs.lib.licenses.mit;
              maintainers = [];
              platforms = pkgs.lib.platforms.unix;
            };
          };
        });

      devShells = forAllSystems (system:
        let pkgs = nixpkgs.legacyPackages.${system};
        in {
          default = pkgs.mkShell {
            packages = [ pkgs.chez ];
            shellHook = ''
              export CHEZSCHEMELIBDIRS="${self}/src"
              export PATH="${self}/bin:$PATH"
              echo "scheme-lint development environment"
              echo "CHEZSCHEMELIBDIRS=$CHEZSCHEMELIBDIRS"
              echo "Run: scheme-lint --help"
            '';
          };
        });
    };
}
