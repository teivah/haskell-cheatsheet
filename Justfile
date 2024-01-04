build:
  ghcid --command="stack ghci Main.hs" --test=":main"

format:
  find . -name '*.hs' -exec hindent {} \;

xp:
  ghci Experiment.hs