#!/usr/bin/fish

# variables

set -gx LANG en_US.utf-8
set -gx VAGRANT_DEFAULT_PROVIDER virtualbox
set -gx SWT_GTK3 0

set -gx EDITOR "emacsclient -t"
set -gx VISUAL "emacsclient -t"

# paths

set -x GOPATH '/home/jhowell/go'

set -x PATH $PATH /home/jhowell/.cargo/bin

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

	# PWD
	set_color $fish_color_cwd
	echo -n (prompt_pwd)
	set_color normal

	echo -n ' $ '

	set_color normal
end

# code

setxkbmap -option "ctrl:nocaps"
