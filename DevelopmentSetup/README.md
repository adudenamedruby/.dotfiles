Development Environment Setup Runbook
=====================================

## Main Dev Things
- Fresh install for love and then sign in to Apple accounts. Finalize by tuning System Preferences.
- Download and install:
    - [Alfred](www.alfredapp.com)
    - [Dash](https://kapeli.com/dash)
    - [iTerm 2](https://www.iterm2.com/downloads.html)
- Download from App Store
    - Xcode (open once installed for tools)
    - 1Password (also get safari extension)
    - Magnet
    - The Unarchiver

## Homebrew and Other Tools
- First, install homebrew
```/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"```
- Git `brew install git`
- zsh `brew install zsh zsh-completions`
    - Once complete, set it to be default shell `chsh -s $(which zsh)
- Vim `brew install --override-system-vi vim`
    - using `which vim`, verify that you're sourcing `usr/local/bin/vim` before `usr/bin/vim`
- Install Python 3: `brew install python3`
- Update .vimrc: `curl https://raw.githubusercontent.com/rouxbuciu/toolPreferences/master/vimrc > .vimrc`


## Firvolous Stuff
- Download Xcode themes https://github.com/hdoria/xcode-themes
    - Go to `~/Library/Developer/Xcode/UserData/FontAndColorThemes/` (FontAndColoThemes may not exist.)
    - Drop in .dvtcolortheme files here. Restart Xcode and your themes should be there.
- Wouldn't it be great if we could see our `$ git log` in vim? Gotta alias `gitlog = git log | vim -R -`


To activate these completions, add the following to your .zshrc:

  fpath=(/usr/local/share/zsh-completions $fpath)

You may also need to force rebuild `zcompdump`:

  rm -f ~/.zcompdump; compinit

Additionally, if you receive "zsh compinit: insecure directories" warnings when attempting
to load these completions, you may need to run this:

  chmod go-w '/usr/local/share'
