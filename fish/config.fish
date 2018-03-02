#set -x PATH $PATH /home/jason/.gem/ruby/2.4.0/bin
set -x PATH $PATH /home/jason/.gem/ruby/2.5.0/bin

set -x PATH $PATH /opt/cocos2d-x/tools/cocos2d-console/bin
set -x PATH $PATH /opt/cocos2d-x/tools/cocos2d-console/plugins/plugin_package
set -x PATH $PATH /opt/cocos2d-x/tools/cocos2d-console/bin

set -x PATH $PATH /opt
set -x PATH $PATH /opt/cocos2d-x/templates

set -x PATH $PATH /home/jason/.cargo/bin

set -x GOPATH /home/jason/go

set -x ANDROID_HOME /home/jason/Android/Sdk

alias android-sdk-manager "java -Xmx256M -Dcom.android.sdkmanager.toolsdir=/home/jason/Android/Sdk/tools -classpath /home/jason/Android/Sdk/tools/lib/sdkmanager.jar:/home/jason/Android/Sdk/tools/lib/swtmenubar.jar:/home/jason/Android/Sdk/tools/lib/x86_64/swt.jar com.android.sdkmanager.Main sdk"
alias recent-branches "git for-each-ref --sort=committerdate refs/heads/ --format='%(HEAD) %(color:yellow)%(refname:short)%(color:reset) - %(color:red)%(objectname:short)%(color:reset) - %(contents:subject)'"

alias docker-rm-exited "docker rm (docker ps -a -q -f status=exited)"
alias docker-rmi-nones "docker rmi (docker images | grep '<none>' | awk '{print $3}')"
alias ktl kubectl

alias pcsx2 PCSX2

setxkbmap -option "ctrl:nocaps"

