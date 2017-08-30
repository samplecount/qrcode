{ghc}:
with (import <nixpkgs> {});

haskell.lib.buildStackProject {
  inherit ghc;
  name = "locosonic-qrencode";
  buildInputs = [
    darwin.apple_sdk.frameworks.Cocoa
    darwin.apple_sdk.frameworks.CoreServices
    pango
    pkgconfig
    qrencode
    zlib
  ];
}
