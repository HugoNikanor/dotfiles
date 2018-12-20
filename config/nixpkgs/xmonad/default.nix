{ nixpkgs ? import <nixpkgs> {} }:

nixpkgs.xmonad-with-packages.override {
	packages = hPkgs: with hPkgs; [
		xmonad-contrib hostname xmobar
		];
}
