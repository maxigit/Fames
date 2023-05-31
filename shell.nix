{ghc?null}:
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
               ] ;
    barcode-libs = [qrencode];
    dev-inputs = [
                 pkg-config
                 # to compile FAY
                 haskellPackages.cpphs
                 ghc
                 which # to find the executable using stack exec which 
               ];
    inputs = runtime-inputs
             ++ (if  ghc == null
                then []
                else dev-inputs);

      
in if ghc == null
   then runCommand "myEnv" {} ''''
   else 
      haskell.lib.buildStackProject {
      inherit ghc;
      name = "myEnv";
      buildInputs = inputs;
   }


