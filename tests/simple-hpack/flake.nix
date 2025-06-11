{
  inputs.garnix-haskell.url = "path:../..";
  outputs = { garnix-haskell, ... }: garnix-haskell.configure { root = ./.; };
}
