#!/bin/sh

setup () {
	echo "++++++++++++++++++++++++++++++++++++++++"
	echo "        Roux's Bootstrap Utility"
	echo "++++++++++++++++++++++++++++++++++++++++"
	echo "\nWould you like to bootstrap this system? (y/n)"
	read resp
	if [ "$resp" = 'y' -o "$resp" = 'Y' ] ; then
		echo "Cloning dotfile repo."
		git clone https://github.com/rouxbuciu/.dotfiles.git
    sh .dotfiles/Installation/bootstrap.sh
	else
		echo "Aborting installation."
	fi
}

setup
