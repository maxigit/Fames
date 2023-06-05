{stack_ghc?null}:
with (import (builtins.fetchTarball {
             name = "fames";
             url = "https://github.com/nixos/nixpkgs/archive/${import ./.nixpkgs}.tar.gz";
             }) {});
let glabels-qr = glabels.overrideAttrs (oldAttrs: {nativeBuildInputs = oldAttrs.nativeBuildInputs ++ barcode-libs;});
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


