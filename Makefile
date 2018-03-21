# why use a makefile and not a shell script? I dunno! Just seemed right.

emacs.d: ~/.emacs.d/init.el
	cp $< emacs.d/init.el

i3: ~/.config/i3/config 
	cp $< i3/config

fish: ~/.config/fish/config.fish
	cp $< fish/config.fish

all: emacs.d fish i3
