{ mkDerivation, base, containers, directory, filepath, hspec
, MissingH, process, stdenv, transformers, unix
}:
mkDerivation {
  pname = "hdotfiles";
  version = "0.2.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base containers directory filepath MissingH process transformers
    unix
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base containers directory filepath hspec transformers
  ];
  homepage = "http://github.com/ilya-yurtaev/hdotfiles#readme";
  description = "Simple dotfiles management tool";
  license = stdenv.lib.licenses.mit;
}
