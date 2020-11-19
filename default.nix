{ compiler ? "ghc883", pkgs ? import <nixpkgs> {} }:

let
	haskellPackages = pkgs.haskell.packages.${compiler};
	drv = haskellPackages.callCabal2nix "adventOfCode2019" ./. {};
in {
	adventOfCode2019 = drv;
	adventOfCode2019-shell = haskellPackages.shellFor {
		packages = p: [drv];
		buildInputs = with pkgs; [
		];
	};
}
