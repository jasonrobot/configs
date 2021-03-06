#!/usr/bin/fish

# variables

set -gx LANG en_US.utf-8
set -gx VAGRANT_DEFAULT_PROVIDER virtualbox
set -gx SWT_GTK3 0

set -gx ELM_SCALE 1.5
set -gx GDK_SCALE 2
set -gx GDK_DPI_SCALE 2

set -gx EDITOR "emacsclient -t"
set -gx VISUAL "emacsclient -t"

# paths

set -x LUA_PATH '/home/jhowell/.luarocks/share/lua/5.3/?.lua;/home/jhowell/.luarocks/share/lua/5.3/?/init.lua;/usr/share/lua/5.3/?.lua;/usr/share/lua/5.3/?/init.lua;/usr/lib/lua/5.3/?.lua;/usr/lib/lua/5.3/?/init.lua;./?.lua;./?/init.lua'
set -x LUA_CPATH '/home/jhowell/.luarocks/lib/lua/5.3/?.so;/usr/lib/lua/5.3/?.so;/usr/lib/lua/5.3/loadall.so;./?.so'
set -x GOPATH '/home/jhowell/go'

# set -x PATH $PATH /home/jhowell/.gem/ruby/2.5.0/bin

set -x PATH $PATH /home/jhowell/.cargo/bin

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



