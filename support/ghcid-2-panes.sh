tmux split-window ghcid -c 'ghci Vultr.hs' -r
tmux split-window ghcid -r -c 'ghci VultrMetaServant.hs' --reload Vultr.hs
