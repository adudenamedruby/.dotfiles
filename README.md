# What dis?

Here you'll find my dotfiles, my setup/keybinding preferences, and a fun script for the whole thing. All nice and neat. It's a work in progress, mostly.

**Important:** if for some reason you see fit to try some of these things out, please fork the repo and remove things you don't want or need before using them. Don’t blindly use my settings unless you know what a thing does. Don't blame me if something goes wrong. Or do, really. But I won't feel sorry. :P

# Installation

### First things first
- Take a moment and appreciate what an unadulterated, fresh mac looks like. Yeah, it's pretty neat. Let's get to the nitty-gritty.
- Sign in to Apple accounts; set System Preferences.
- Download and install the important stuff:
    - [Alfred](www.alfredapp.com)
    - [iTerm 2](https://www.iterm2.com/downloads.html) - set up hotkey shortcut Cmd-Ctrl-T
    - [Karabiner](https://pqrs.org/osx/karabiner/)
- Download from App Store
    - Xcode
      - Once installed, open to install tools!
    - 1Password (also get safari/crome extension)
    - Magnet (and set hotkeys)
    - Tweetbot
    - Spotify

### Let's get Phys... no. Let's get automated!
- In iTerm `git clone https://github.com/rouxbuciu/.dotfiles.git` in the home directory
- Then `cd .dotfiles` & `bootstrap.exclude.sh` & grab some hangtime because this'll take a while.
- Once this has run, quit terminal and reboot computer for a good time!

### XVim - why settle for less!?
- Rename standard xcode to Xcode-Distribution (if you need a distribution Xcode)
- Download another version of Xcode [from Apple](https://developer.apple.com/download/more/)
- Sign your own [code signing certificate](https://github.com/XVimProject/XVim2/blob/master/SIGNING_Xcode.md)
- Install [XVim2](https://github.com/XVimProject/XVim2)

### Spacemacs
- Spacemacs install instructions: https://github.com/syl20bnr/spacemacs (Install dev branch (if stable))

### Almost done!
- Run `brew doctor` to see any issues and fix them as needed
- For further **ZSH** customization:
    - Install [oh-my-zsh](https://github.com/robbyrussell/oh-my-zsh)
    - Install **Powerlevel9x** `git clone https://github.com/bhilburn/powerlevel9k.git ~/.oh-my-zsh/custom/themes/powerlevel9k`
    - Install **zsh-syntax-highlighting** `git clone https://github.com/zsh-users/zsh-syntax-highlighting.git ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-syntax-highlighting`
    - Install **zsh-auto-completion** `git clone https://github.com/zsh-users/zsh-autosuggestions ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-autosuggestions`
    - In .zshrc watch line 5! Make sure to put in your current username, rather than `acmelabs`
- More **VIM** stuff
    - Using `which vim`, verify that you're sourcing `usr/local/bin/vim` before `usr/bin/vim`
    - Install [Vim-Plug](https://github.com/junegunn/vim-plug) `curl -fLo ~/.vim/autoload/plug.vim --create-dirs \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim`
    - open vim and run `:PlugInstall` to install plugins
- Xcode themes from [here](https://github.com/hdoria/xcode-themes) and [here](http://www.codethemes.net/themes/popular/all) should go in `~/Library/Developer/Xcode/UserData/FontAndColorThemes/` (FontAndColoThemes may not exist.)
- [iTerm2 Themes](https://github.com/mbadolato/iTerm2-Color-Schemes)
- One last restart!

### Other
- In chrome, don't forget to install `vimium`
- In Alfred, set up various workflows or shortcuts you need.
- Maybe get [Dash](https://kapeli.com/dash)

# Why a dotfiles repo?

Inspired by [mathiasbyens'](https://github.com/mathiasbynens/dotfiles) and others, the dotfile repo is a great way to keep all your preferences the same across all dev environments! Tasty! furthermore.... it helps get a system going faster! Finally, and most importantly, symlinked files are <3.

# Why share?
Sharing is caring. We learn from each other 🌷

# License

The contents of this repo are licensed under the [MIT license](https://opensource.org/licenses/MIT).

# Contributing

If you have any ideas or suggestions, feel free to open up an issue or shoot through a pull request! Thanks!

Feel free to fork whenever you want!
