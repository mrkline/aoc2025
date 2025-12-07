let pkgs = (import <nixos> {});
in pkgs.mkShell {
  packages = [
    (pkgs.haskellPackages.ghcWithPackages (p: with p; [
        bytestring
        data-interval
        mtl
        linear
        parallel
        split
        text
        vector
    ]))
  ];
}
