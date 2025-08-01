{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/4e238e4aa894a139332b3d496a54f35a71180d5c.tar.gz") {} }:

pkgs.mkShell {
  packages =
    let elm = pkgs.elmPackages; in
        [
        pkgs.nodejs_22
        elm.elm
        elm.elm-test
        elm.elm-format
    ];
}
