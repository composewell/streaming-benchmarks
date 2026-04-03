{
  description = "Streaming-benchmarks";

  inputs = {
    basepkgs.url = "git+ssh://git@github.com/composewell/streamly-packages?rev=0f2afe624cb0e259edbc87bdf60aabdd4cf2580a";
    nixpkgs.follows = "basepkgs/nixpkgs";
    nixpkgs-darwin.follows = "basepkgs/nixpkgs-darwin";
  };

  outputs = { self, nixpkgs, nixpkgs-darwin, basepkgs }:
    basepkgs.nixpack.mkOutputs {
      inherit nixpkgs nixpkgs-darwin basepkgs;
      name = "streaming-benchmarks";
      sources = import ./sources.nix;
      packages = import ./packages.nix;
    };
}
