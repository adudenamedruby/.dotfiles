#!/bin/sh

# This is for symlinking the required files

dotfiles () {
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
        ln -sv "$PWD/RouxAlternateBindings.idekeybindings" "$HOME/Library/Developer/Xcode/UserData/Keybindings/"
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
            ln -sv "$PWD/karabiner.json" "$HOME/.config/karabiner/"
            echo "Karabiner symlinking complete"
        fi
    else
        echo "Skipping Karabiner keybinding symlinks"
    fi
}

dotfiles
xcodeKeybindings
karabiner
