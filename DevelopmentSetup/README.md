Development Environment Setup Runbook
=====================================

## Main Dev Things
- Fresh install for love and then sign in to Apple accounts. Finalize by setting System Preferences.
- Download and install:
    - [Alfred](www.alfredapp.com)
    - [Dash](https://kapeli.com/dash)
    - [iTerm 2](https://www.iterm2.com/downloads.html)
- Download from App Store
    - Xcode (open, once installed, to install tools)
    - 1Password (also get safari extension)
    - Magnet
    - The Unarchiver

## Homebrew and Other Tools
- Install **Homebrew**
```/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"```
- **Git** `brew install git`
- [**ZSH** install instructions](https://github.com/robbyrussell/oh-my-zsh/wiki/Installing-ZSH)
    - If encountering trouble [here's how to use Homebrew's zsh as system zsh](https://rick.cogley.info/post/use-homebrew-zsh-instead-of-the-osx-default/)
    - Install of [oh-my-zsh](https://github.com/robbyrussell/oh-my-zsh)
    - Copy .zshrc from the repo to the local .zshrc
        - Watch line 5! Make sure to put in your current username, rather than acmelabs
    - Also install `git clone https://github.com/zsh-users/zsh-syntax-highlighting` Don't forget to add `zsh-syntax-highlighting` to the .zshrc plugins section
- **Vim** `brew install vim --with-override-system-vi`
    - Restart terminal
    - using `which vim`, verify that you're sourcing `usr/local/bin/vim` before `usr/bin/vim`
    - .vimrc: `curl https://raw.githubusercontent.com/rouxbuciu/toolPreferences/master/vimrc > .vimrc`
- **Python 3** `brew install python3`
- **openssh** `brew install openssh`
- **grep** `brew install grep --with-default-names`
- Restart terminal
- Disable Notification Center and remove the menu bar icon `launchctl unload -w /System/Library/LaunchAgents/com.apple.notificationcenterui.plist 2> /dev/null`
- Expand save panel by default
```
defaults write NSGlobalDomain NSNavPanelExpandedStateForSaveMode -bool true
defaults write NSGlobalDomain NSNavPanelExpandedStateForSaveMode2 -bool true
```
- Don’t display the annoying prompt when quitting iTerm `defaults write com.googlecode.iterm2 PromptOnQuit -bool false`
- End setup by running `brew cleanup' to clean out any old formulas


## Firvolous Stuff
- Download Xcode themes https://github.com/hdoria/xcode-themes
    - Go to `~/Library/Developer/Xcode/UserData/FontAndColorThemes/` (FontAndColoThemes may not exist.)
    - Drop in .dvtcolortheme files here. Restart Xcode and your themes should be there.
- Wouldn't it be great if we could see our `$ git log` in vim? Gotta alias `gitlog = git log | vim -R -` in .zshrc
