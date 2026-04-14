{nixpack}:
with nixpack.mkSources;
{
layers = [
{
  streaming-benchmarks = local ./.;
  #streaming-benchmarks = localOpts ./.
    #[]
    #["--flags streaming"];
  fusion-plugin =
    gh "composewell" "fusion-plugin" "15c0ad50a235a35e85d03cced9ad0f3938565e5a";
}
];
}
