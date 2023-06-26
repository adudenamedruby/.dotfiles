#!/bin/sh

# This is for symlinking the required files

dotfiles () {
    echo "Symlink preference dotfiles? (y/n)"
    read resp
    if [ "$resp" = 'y' -o "$resp" = 'Y' ] ; then
        ln -s ~/.dotfiles/RC_Files/.* ~/
        echo "Dotfile symlinking complete."
    else
        echo "Skipping dotfiles symlinks"
    fi
}

xcodePrefs () {
    echo "Symlink XCode preferences? (y/n)"
    read resp
    if [ "$resp" = 'y' -o "$resp" = 'Y' ] ; then
        ln -s ~/.dotfiles/XcodePreferences/CodeSnippets/ ~/Library/Developer/Xcode/UserData/
        ln -s ~/.dotfiles/XcodePreferences/FontAndColorThemes/ ~/Library/Developer/Xcode/UserData/
        mkdir ~/Library/Developer/Xcode/UserData/KeyBindings
        ln -s ~/.dotfiles/Keyboard_Preferences/RouxsNightmare.idekeybindings ~/Library/Developer/Xcode/UserData/KeyBindings/
        echo "Xcode symlinking complete"
    else
        echo "Skipping Xcode keybinding symlinks"
    fi
}

karabiner () {
    echo "Symlink Karabiner Keybindings? (y/n)"
    read resp
    if [ "$resp" = 'y' -o "$resp" = 'Y' ] ; then
        mkdir ~/.config/karabiner/
        touch ~/.config/karabiner/karabiner.json
        ln -s ~/.dotfiles/Keyboard_Preferences/karabiner.edn ~/.config
        cd ~/.config
        goku
    else
        echo "Skipping Karabiner keybinding symlinks"
    fi
}

dotfiles
xcodePrefs
karabiner
