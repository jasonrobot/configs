# why use a makefile and not a shell script? I dunno! Just seemed right.

emacs: ~/.emacs.d/*.el
	cp -t emacs.d $^

i3: ~/.config/i3/config
	cp $< i3/config

fish: ~/.config/fish/config.fish
	cp $< fish/config.fish

all: emacs fish i3
