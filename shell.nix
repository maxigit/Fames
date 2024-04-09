{ghc?null}:
let pkgs =  (import (builtins.fetchTarball {
             name = "fames";
             url = "https://github.com/nixos/nixpkgs/archive/${import ./.nixpkgs}.tar.gz";
             }) {});
    stack_ghc = ghc;
in if stack_ghc != null && stack_ghc.version != pkgs.ghc.version
   then abort ("stack ghc " + stack_ghc.version + " different from " + pkgs.ghc.version)
   else with pkgs;
let ghc = pkgs.ghc ;
    glabels-qr = glabels.overrideAttrs (oldAttrs: {nativeBuildInputs = oldAttrs.nativeBuildInputs ++ barcode-libs;});
    runtime-inputs = [ 
                    curl
                    openssl
                    libmysqlclient
                    pcre
                    zlib
                    glib
                    cairo
                    pango
                    glabels-qr
                    ncurses6
		   barcode
               ] ;
    barcode-libs = [qrencode ];
    dev-inputs = [
                 pkg-config
                 # to compile FAY
                 haskellPackages.cpphs
                 ghc
                 which # to find the executable using stack exec which 
               ];
    inputs = runtime-inputs
             ++ (if  stack_ghc == null
                then []
                else dev-inputs);

      
in if stack_ghc == null
   then mkShell {
	packages=[coreutils which];
        inputsFrom = [ghc];
        buildInputs = inputs;
        }
   else 
      haskell.lib.buildStackProject {
      inherit ghc;
      name = "myEnv";
      buildInputs = inputs;
   }
