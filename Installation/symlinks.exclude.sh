#!/bin/sh

# This is for symlinking the required files

dotfiles () {
    echo "Symlink preference dotfiles? (y/n)"
    read resp
    if [ "$resp" = 'y' -o "$resp" = 'Y' ] ; then
        cd ..
	      cd RC_Files
        for file in $( ls -A | grep -vE '\.exclude*|\.git$|\.gitignore|.*.md|.*.ino|.*.idekeybindings|.*.json' ) ; do
                    if [[ -f "~/${file##*/}" ]]; then
                        echo "File $file already exists. Remove it? [y/n]"
                        read resp
                        if [ "$resp" = 'y' -o "$resp" = 'Y' ] ; then
                                rm ~/$file
                        fi
                    fi
            ln -s $file ~/
        done
        echo "Dotfile symlinking complete."
	cd ..
	cd Installation
    else
        echo "Skipping dotfiles symlinks"
    fi
}

xcodeKeybindings () {
    echo "Symlink XCode Keybindings? (y/n)"
    read resp
    if [ "$resp" = 'y' -o "$resp" = 'Y' ] ; then
        if [ -e ~/Library/developer/Xcode/UserData/Keybindings/RouxAlternateBindings.idekeybindings ]; then
            echo "Keybindings file already exists. Remove it? [y/n]"
            read resp
            if [ "$resp" = 'y' -o "$resp" = 'Y' ] ; then
                    rm ~/Library/developer/Xcode/UserData/Keybindings/RouxAlternateBindings.idekeybindings
            fi
        ln -sv RouxAlternateBindings.idekeybindings ~/Library/Developer/Xcode/UserData/Keybindings/
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
        if [ -e ~/.config/karabiner/karabiner.json ]; then
            echo "Keybindings file already exists. Remove it? [y/n]"
            read resp
            if [ "$resp" = 'y' -o "$resp" = 'Y' ] ; then
                    rm ~/.config/karabiner/karabiner.json
            fi
            ln -sv karabiner.json ~/.config/karabiner/
            echo "Karabiner symlinking complete"
        fi
    else
        echo "Skipping Karabiner keybinding symlinks"
    fi
}

dotfiles
xcodeKeybindings
karabiner
