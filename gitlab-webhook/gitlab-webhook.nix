{ mkDerivation, base, lib }:
mkDerivation {
  pname = "gitlab-webhook";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base ];
  homepage = "https://github.com/githubuser/gitlab-webhook#readme";
  license = lib.licenses.bsd3;
  mainProgram = "gitlab-webhook-exe";
}
