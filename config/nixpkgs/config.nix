with (import <nixpkgs> {});
{
	# packageOverrides = pkgs_: with pkgs_; {
	# 	xmonad = import ./xmonad { nixpkgs = pkgs_; };
	# };
	packageOverrides = super: let self = super.pkgs; in
	{
		xmonad = import ./xmonad { nixpkgs = self; };
		myHaskellEnv =
			self.haskell.packages.ghc843.ghcWithPackages
			( haskellPackages: with haskellPackages; [
			  hostname yaml HaTeX
			]);
	};
}
