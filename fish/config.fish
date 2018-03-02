#!/usr/bin/fish

# variables

set -gx LANG en_US.utf-8
set -x VAGRANT_DEFAULT_PROVIDER virtualbox
set SWT_GTK3 0

# paths

set -x LUA_PATH '/home/jason/.luarocks/share/lua/5.3/?.lua;/home/jason/.luarocks/share/lua/5.3/?/init.lua;/usr/share/lua/5.3/?.lua;/usr/share/lua/5.3/?/init.lua;/usr/lib/lua/5.3/?.lua;/usr/lib/lua/5.3/?/init.lua;./?.lua;./?/init.lua'
set -x LUA_CPATH '/home/jason/.luarocks/lib/lua/5.3/?.so;/usr/lib/lua/5.3/?.so;/usr/lib/lua/5.3/loadall.so;./?.so'
set -x GOPATH '/home/jason/go'
set -x PATH $PATH /home/jason/.gem/ruby/2.5.0/bin
set -x PATH $PATH /home/jason/.cargo/bin

# aliases and functions

alias docker-rmi-nones "docker rmi (docker images | grep '<none>' | awk '{print $3}')"
alias ktl kubectl
alias ff firefox

function recent-branches
    git for-each-ref --sort=committerdate refs/heads/ --format='%(HEAD) %(color:yellow)%(refname:short)%(color:reset) - %(color:red)%(objectname:short)%(color:reset) - %(contents:subject) - %(authorname) (%(color:green)%(committerdate:relative)%(color:reset))' | tail -n 20
end

function rmscreenshots
    rm screenshots/failure*
end

function docker-rm-exited-containers
    docker rm (docker ps -a -f status=exited -q)
end

function lnh
    ln --help | head -n 1
end

function jsformat
    # only 1 arg allowed though
    js-beautify -P -r $argv
#    cat $argv | awk "BEGIN {} {} END {}
end

# code

setxkbmap -option "ctrl:nocaps"
