#!/bin/sh

init () {
    echo "SYSTEM BOOTSTRAP"
    echo "WARNING: This utility will run the bootstrap scrip to set up your computer."
    echo "Proceed? (y/n)
    read resp
    if [ "$resp" = 'y' -o "$resp" = 'Y' ] ; then
        echo "Geronimooooooooo!!!!"
        developer_mode
        install_tools
        symlink_setup
        download_other
        macOS_preferences
        install_finished
	else
		echo "System bootstrap aborted."
		return 1
	fi
}

developer_mode () {
	echo "Would you like to enable Developer mode on this Mac? (y/n)"
	read resp
	if [ "$resp" = 'y' -o "$resp" = 'Y' ] ; then
		DevToolsSecurity -enable
        echo "Developer mode enabled"
	else
		echo "Developer mode not enabled"
		return 1
	fi
}

symlink_setup () {
	echo "This utility will symlink the files in this repo to the home directory"
	echo "Proceed? (y/n)"
	read resp
	if [ "$resp" = 'y' -o "$resp" = 'Y' ] ; then
		for file in $( ls -A | grep -vE '\.exclude*|\.git$|\.gitignore|.*.md|.*.idekeybindings|.' ) ; do
            if [ -e ~/$file ]; then
                echo "File $file already exists. Remove it? [y/n]"
                read resp
                if [ "$resp" = 'y' -o "$resp" = 'Y' ] ; then
                    rm ~/$file
                fi
            fi
			ln -sv "$PWD/$file" "$HOME"
		done
        ln -sv "$PWD/RouxAlternateBindings.idekeybindings" "$HOME/Library/Developer/Xcode/UserData/Keybindings/"
		echo "Symlinking complete"
	else
		echo "Symlinking cancelled by user"
		return 1
	fi
}

install_tools () {
	if [ $( echo "$OSTYPE" | grep 'darwin' ) ] ; then
		echo "This utility will install useful utilities using Homebrew"
		echo "Proceed? (y/n)"
		read resp
		if [ "$resp" = 'y' -o "$resp" = 'Y' ] ; then
			echo "Installing useful stuff using brew. This may take a while..."
            echo "Maybe brew yourself a coffee?"
			sh brew.exclude.sh
		else
			echo "Brew installation cancelled by user"
		fi
	else
		echo "Skipping installations using Homebrew because macOS was not detected..."
	fi
}

download_other () {
    echo "This utility will download the rest of the files for bootstrapping system"
    echo "Proceed? (y/n)"
    read resp
    if [ "$resp" = 'y' -o "$resp" = 'Y' ] ; then
        echo "Downloading."
        sh download.exclude.sh
    else
        echo "Downloads cancelled by user"
    fi
}

macOS_preferences () {
    echo "This utility will set system preferences to prepared defaults"
    echo "Proceed? (y/n)"
    read resp
    if [ "$resp" = 'y' -o "$resp" = 'Y' ] ; then
        echo "Setting preferences..."
        sh download.exclude.sh
    else
        echo "Setting preferences skipped."
    fi
}

install_finished () {
    echo "System bootstrap has finished."
    echo "Code responsibly."
}

init
