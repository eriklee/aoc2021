AOC 2021
--------

For regular lua
`$ nix-shell -p lua fennel rlwrap`

For luajit + fennel with luajit instead
`$ nix-shell -p luajit 'fennel.override { lua = luajit; }'`
