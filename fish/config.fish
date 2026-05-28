set -x PATH $PATH /home/jason/.gem/ruby/3.0.0/bin

set -x PATH $PATH /home/jason/.cargo/bin

set -x PATH $PATH /home/jason/bin

set -x GOPATH /home/jason/go

set -x EDITOR nano
set -x VISUAL nano

alias docker-rm-exited "docker rm (docker ps -a -q -f status=exited)"
alias docker-rmi-nones "docker images | grep '<none>' | awk '{print $3}' | xargs docker rmi \{\} ;"
alias ktl kubectl

setxkbmap -option "compose:caps"

set always_rlwrap clojure sbcl guile
for name in $always_rlwrap
    alias $name "rlwrap $name"
end

function recent-branches
    git for-each-ref --sort=committerdate refs/heads/ --format='%(HEAD) %(color:yellow)%(refname:short)%(color:reset) - %(color:red)%(objectname:short)%(color:reset) - %(contents:subject) - %(authorname) (%(color:green)%(committerdate:relative)%(color:reset))' | tail -n 20
end

function clip
    xclip -selection clipboard $argv
end

function bring --description 'Move a file and cd to the destination'
    set -l target $argv[1..-2]
    set -l destination $argv[-1]

    mv $target $destination
    cd $destination
end

# fnm env vars
set -gx PATH "/run/user/1000/fnm_multishells/23501_1773462074055/bin" $PATH;
set -gx FNM_MULTISHELL_PATH "/run/user/1000/fnm_multishells/23501_1773462074055";
set -gx FNM_VERSION_FILE_STRATEGY "local";
set -gx FNM_DIR "/home/jason/.local/share/fnm";
set -gx FNM_LOGLEVEL "info";
set -gx FNM_NODE_DIST_MIRROR "https://nodejs.org/dist";
set -gx FNM_COREPACK_ENABLED "false";
set -gx FNM_RESOLVE_ENGINES "true";
set -gx FNM_ARCH "x64";
