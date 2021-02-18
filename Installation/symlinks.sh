#!/bin/sh

# This is for symlinking the required files

dotfiles () {
    echo "Symlink preference dotfiles? (y/n)"
    read resp
    if [ "$resp" = 'y' -o "$resp" = 'Y' ] ; then
        ln -s ~/.dotfiles/RC_Files/* ~/
        echo "Dotfile symlinking complete."
    cd Installation
    else
        echo "Skipping dotfiles symlinks"
    fi
}

xcodeKeybindings () {
    echo "Symlink XCode Keybindings? (y/n)"
    read resp
    if [ "$resp" = 'y' -o "$resp" = 'Y' ] ; then
        ln -s ~/.dotfiles/Keyboard_Preferences/RouxsNightmare.idekeybindings ~/Library/Developer/Xcode/UserData/KeyBindings/
        echo "Xcode symlinking complete"
        fi
    else
        echo "Skipping Xcode keybinding symlinks"
    fi
}

karabiner () {
    echo "Symlink Karabiner Keybindings? (y/n)"
    read resp
    if [ "$resp" = 'y' -o "$resp" = 'Y' ] ; then
        ln -s ~/.dotfiles/Keyboard_Preferences/karabiner.json ~/.config/karabiner/
    else
        echo "Skipping Karabiner keybinding symlinks"
    fi
}

dotfiles
xcodeKeybindings
karabiner
