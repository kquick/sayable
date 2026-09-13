{ mkDerivation, base, bytestring, containers, exceptions, hspec
, lib, prettyprinter, tasty, tasty-ant-xml, tasty-hspec
, template-haskell, text, th-abstraction
}:
mkDerivation {
  pname = "sayable";
  version = "1.2.7.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring containers exceptions prettyprinter
    template-haskell text th-abstraction
  ];
  testHaskellDepends = [
    base hspec prettyprinter tasty tasty-ant-xml tasty-hspec
    template-haskell text
  ];
  description = "Construction of context-adjusted pretty output";
  license = lib.licenses.isc;
}
