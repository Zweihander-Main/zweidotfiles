{
  description = "Nix config";

  inputs = {
    # Nixpkgs
    nixpkgs.url = "github:nixos/nixpkgs/nixos-23.11";
    # nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";

    # Home manager
    home-manager.url = "github:nix-community/home-manager/release-23.11";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    hardware.url = "github:nixos/nixos-hardware";
  };

  outputs = {
    self,
    nixpkgs,
    home-manager,
    ...
  } @ inputs: let
    inherit (self) outputs;
    # Supported systems for your flake packages, shell, etc.
    systems = [
      "aarch64-linux"
      "i686-linux"
      "x86_64-linux"
      "aarch64-darwin"
      "x86_64-darwin"
    ];
    # This is a function that generates an attribute by calling a function you
    # pass to it, with each system as an argument
    forAllSystems = nixpkgs.lib.genAttrs systems;
    # Load secrets file
    secrets = builtins.fromJSON (builtins.readFile "${self}/../secrets/secrets.json");
  in {
    packages = forAllSystems (system: import ./pkgs nixpkgs.legacyPackages.${system});
    formatter = forAllSystems (system: nixpkgs.legacyPackages.${system}.alejandra);
    overlays = import ./overlays {inherit inputs;};

    nixosModules = import ./lib/nixos;
    homeManagerModules = import ./lib/home-manager;

    # NixOS configuration entrypoint
    # Available through 'nixos-rebuild --flake .#aethelweard'
    nixosConfigurations = {
      aethelweard = nixpkgs.lib.nixosSystem {
        specialArgs = {inherit inputs outputs secrets;};
        modules = [./hosts/aethelweard];
      };
      droid = nixpkgs.lib.nixosSystem {
        specialArgs = {inherit inputs outputs secrets;};
        modules = [./hosts/droid];
      };
    };

    # Standalone home-manager configuration entrypoint
    # Available through 'home-manager --flake .#zwei@aethelweard'
    homeConfigurations = {
      "zwei@server" = home-manager.lib.homeManagerConfiguration {
        pkgs = nixpkgs.legacyPackages.x86_64-linux;
        extraSpecialArgs = {inherit inputs outputs secrets;};
        modules = [./hosts/server/lib.nix ./home/zwei/server.nix];
      };
      "zwei@desktop" = home-manager.lib.homeManagerConfiguration {
        pkgs = nixpkgs.legacyPackages.x86_64-linux;
        extraSpecialArgs = {inherit inputs outputs secrets;};
        modules = [./hosts/desktop/lib.nix ./home/zwei/desktop.nix];
      };
      "zwei@ptah" = outputs.homeConfigurations."zwei@desktop";
      "zwei@horus" = outputs.homeConfigurations."zwei@desktop";
      "zwei@aethelweard" = home-manager.lib.homeManagerConfiguration {
        pkgs = nixpkgs.legacyPackages.x86_64-linux;
        extraSpecialArgs = {inherit inputs outputs secrets;};
        modules = [./hosts/aethelweard/lib.nix ./home/zwei/nixos.nix];
      };
      "karlmagnus@aethelweard" = home-manager.lib.homeManagerConfiguration {
        pkgs = nixpkgs.legacyPackages.x86_64-linux;
        extraSpecialArgs = {inherit inputs outputs secrets;};
        modules = [./home/karlmagnus/default.nix];
      };
    };
  };
}
