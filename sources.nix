{nixpack}:
with nixpack.mkSources;
{
layers = [
{
  streaming-benchmarks = local ./.;
  #streaming-benchmarks = localOpts ./.
    #[]
    #["--flags streaming"];
}
];
}
