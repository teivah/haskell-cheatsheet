build:
  ghcid --command="stack ghci Main.hs" --test=":main"

format:
  hindent Main.hs

play:
  ghci

ghci:
  ghci Experiment.hs