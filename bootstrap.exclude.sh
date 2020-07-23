#!/bin/sh

init () {
    echo "SYSTEM BOOTSTRAP"
    echo "WARNING: This utility will run the bootstrap scrip to set up your computer."
    echo "Proceed? (y/n)"
    read resp
    if [ "$resp" = 'y' -o "$resp" = 'Y' ] ; then
        echo "Geronimooooooooo!!!!"
        developer_mode
        install_tools
        setup_utilities
        symlink_setup
        fetch_repos
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
    fi
}

symlink_setup () {
    echo "This utility will symlink the files in this repo to the appropriate directories"
    echo "Proceed? (y/n)"
    read resp
    if [ "$resp" = 'y' -o "$resp" = 'Y' ] ; then
        sh symlinks.exclude.sh
        echo "Symlinking process complete."
    else
        echo "Symlinking cancelled by user"
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

setup_utilities () {
    echo "This utility will download and install the rest of the utilities for bootstrapping"
    echo "Proceed? (y/n)"
    read resp
    if [ "$resp" = 'y' -o "$resp" = 'Y' ] ; then
        echo "Downloading..."
        sh downloads.exclude.sh
    else
        echo "Skipping miscellaneous file downloads."
    fi
}

fetch_repos () {
    echo "This utility will fetch roux's personal git repos and put them in the ~/code/src/ folder"
    echo "Proceed? (y/n)"
    read resp
    if [ "$resp" = 'y' -o "$resp" = 'Y' ] ; then
        echo "Creating folders"
        mkdir ~/code
        mkdir ~/code/src
        echo "Fetching repos:"
        sh fetchPersonalRepos.exclude.sh
        echo "Repo fetch complete."
    else
        echo "Setting preferences skipped."
    fi
}

macOS_preferences () {
    echo "This utility will set system preferences to prepared defaults"
    echo "Proceed? (y/n)"
    read resp
    if [ "$resp" = 'y' -o "$resp" = 'Y' ] ; then
        echo "Setting preferences..."
        sh macos.exclude.sh
    else
        echo "Setting preferences skipped."
    fi
}

install_finished () {
    echo "System bootstrap has finished."
    echo "Please restart your system."
    echo "==========================="
    echo "Code responsibly."
}

init
