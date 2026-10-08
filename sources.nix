{nixpack}:
with nixpack.mkSources;
{
layers = [
{
  streamly-lz4 = local ./.
    // { c2nix = ["--flag fusion-plugin"];
         flags = ["--flags fusion-plugin"];
       };
}
];
}
