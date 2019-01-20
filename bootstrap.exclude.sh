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
        symlink_dotfiles
        symlink_xcodeKeybindings
        symlink_karabiner
        symlink_alfredPreferences
        echo "Symlinking process complete."
    else
        echo "Symlinking cancelled by user"
    fi
}

symlink_dotfiles () {
    echo "Symlink preference dotfiles? (y/n)"
    read resp
    if [ "$resp" = 'y' -o "$resp" = 'Y' ] ; then
        for file in $( ls -A | grep -vE '\.exclude*|\.git$|\.gitignore|.*.md|.*.idekeybindings|.*.json' ) ; do
                    if [ -e ~/$file ]; then
                        echo "File $file already exists. Remove it? [y/n]"
                        read resp
                        if [ "$resp" = 'y' -o "$resp" = 'Y' ] ; then
                                rm ~/$file
                        fi
                    fi
            ln -sv "$PWD/$file" "$HOME"
        done
            echo "Dotfile symlinking complete."
    else
        echo "Skipping dotfiles symlinks"
    fi
}

symlink_xcodeKeybindings () {
    echo "Symlink XCode Keybindings? (y/n)"
    read resp
    if [ "$resp" = 'y' -o "$resp" = 'Y' ] ; then
            ln -sv "$PWD/RouxAlternateBindings.idekeybindings" "$HOME/Library/Developer/Xcode/UserData/Keybindings/"
        echo "Xcode symlinking complete"
    else
        echo "Skipping Xcode keybinding symlinks"
    fi
}

symlink_karabiner () {
    echo "Symlink Karabiner Keybindings? (y/n)"
    read resp
    if [ "$resp" = 'y' -o "$resp" = 'Y' ] ; then
            ln -sv "$PWD/karabiner.json" "$HOME/.config/karabiner/"
        echo "Karabiner symlinking complete"
    else
        echo "Skipping Karabiner keybinding symlinks"
    fi
}

symlink_alfredPreferences () {
    echo "Symlink Alfred preferences? (y/n)"
    read resp
    if [ "$resp" = 'y' -o "$resp" = 'Y' ] ; then
        mkdir ~/code/
        mkdir ~/code/Alfred/
            ln -sv "$PWD/Alfred.preferences" "~/code/Alfred/"
        echo "Alfred preferences symlinking complete"
    else
        echo "Skipping Alfred preferences symlinks"
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
        sh download.exclude.sh
    else
        echo "Skipping miscellaneous file downloads."
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
