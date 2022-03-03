#!/usr/bin/fish

# variables

set -gx LANG en_US.utf-8
set -gx VAGRANT_DEFAULT_PROVIDER virtualbox
set -gx SWT_GTK3 0

# set -gx ELM_SCALE 1.5
# set -gx GDK_SCALE 1
# set -gx GDK_DPI_SCALE 2

set -gx EDITOR 'emacsclient -t'
set -gx VISUAL 'emacsclient -t'

# paths

# set -x LUA_PATH '/home/jhowell/.luarocks/share/lua/5.3/?.lua;/home/jhowell/.luarocks/share/lua/5.3/?/init.lua;/usr/share/lua/5.3/?.lua;/usr/share/lua/5.3/?/init.lua;/usr/lib/lua/5.3/?.lua;/usr/lib/lua/5.3/?/init.lua;./?.lua;./?/init.lua'
# set -x LUA_CPATH '/home/jhowell/.luarocks/lib/lua/5.3/?.so;/usr/lib/lua/5.3/?.so;/usr/lib/lua/5.3/loadall.so;./?.so'
set -x GOPATH '/home/jhowell/go'

# set -x PATH $PATH /home/jhowell/.gem/ruby/2.5.0/bin

set -x PATH $PATH '/home/jhowell/.cargo/bin'
set -x PATH $PATH '/home/jhowell/.deno/bin'

set -x DENO_INSTALL '/home/jhowell/.deno'


set -gx RIPGREP_CONFIG_PATH ~/.ripgreprc

# aliases and functions

alias docker-rmi-nones "docker rmi (docker images | grep '<none>' | awk '{print $3}')"
alias ktl kubectl
alias ff firefox

function docker-rm-exited-containers
    docker rm (docker ps -a -f status=exited -q)
end

alias eom eog

function jsformat
    # only 1 arg allowed though
    js-beautify -P -r $argv
#    cat $argv | awk "BEGIN {} {} END {}
end

function lnh
    ln --help | head -n 1
end

function mkcd
    mkdir -p $argv[1]
    cd $argv[1]
end

function recent-branches
    git for-each-ref --sort=committerdate refs/heads/ --format='%(HEAD) %(color:yellow)%(refname:short)%(color:reset) - %(color:red)%(objectname:short)%(color:reset) - %(contents:subject) - %(authorname) (%(color:green)%(committerdate:relative)%(color:reset))' | tail -n 20
end

function clip
    xclip -selection clipboard $argv
end

function fish_prompt --description 'Write out the prompt'
	if not set -q __fish_prompt_normal
		set -g __fish_prompt_normal (set_color normal)
	end

    # start with a newline
    echo ''

	# PWD
	set_color $fish_color_cwd
	echo -n (prompt_pwd)
	set_color normal

	echo -n ' $ '

	set_color normal
end

function bring --description 'Move a file and cd to the destination'
    set -l target $argv[1..-2]
    set -l destination $argv[-1]

    mv $target $destination
    cd $destination
end

function rgcomp --description 'Find usages of components by addComponent'
    rg -tjs -tts "^\s*type:.*$argv[1]"
    rg -tjs -tts "new ontrapot\.Components.+$argv[1]"
end

# code

setxkbmap -option "ctrl:nocaps"

