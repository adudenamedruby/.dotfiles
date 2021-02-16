#!/bin/sh

setup () {
	echo "++++++++++++++++++++++++++++++++++++++++"
	echo "        Roux's Bootstrap Utility"
	echo "++++++++++++++++++++++++++++++++++++++++"
	echo "\nWould you like to bootstrap this system? (y/n)"
	read resp
	if [ "$resp" = 'y' -o "$resp" = 'Y' ] ; then
		echo "Cloning into stuff!!!!"
		#git clone https://github.com/rouxbuciu/.dotfiles.git
	else
		echo "Aborting installation"
	fi
}

setup
