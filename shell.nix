{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, containers, directory, filepath, hspec
      , MissingH, process, stdenv, temporary, transformers, unix
      }:
      mkDerivation {
        pname = "hdotfiles";
        version = "0.3.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          base containers directory filepath MissingH process transformers
          unix
        ];
        executableHaskellDepends = [ base ];
        testHaskellDepends = [
          base containers directory filepath hspec temporary transformers
          unix
        ];
        homepage = "http://github.com/ilya-yurtaev/hdotfiles#readme";
        description = "Simple dotfiles management tool";
        license = stdenv.lib.licenses.mit;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
