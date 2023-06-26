# adudenamedruby's dotfiles

## Why a dotfiles repo?

Inspired by various members of Github, the dotfile repo is a great way to keep all your preferences the same across all dev environments! Tasty!

Furthermore.... it helps get a system going faster from fresh install to ready for development.

It's a work in progress, however, and is likely to change frequently.

**Important:** if for some reason you see fit to try some of these things out, please fork the repo and remove things you don't want or need before using them. Don’t blindly use my settings unless you know what a thing does - a lot of this stuff is idiosyncratic to my workflow. Don't blame me if something goes wrong, or behaves how you don't like it. You have been warned.

# Installation

### First things first
- Take a moment and appreciate what an unadulterated, fresh mac looks like. Yeah, it's pretty neat. Let's get to the nitty-gritty.
- Sign in to Apple accounts; set System Preferences.
- Download and install the important stuff:
    - [iTerm 2](https://www.iterm2.com/downloads.html) - set up hotkey shortcut Cmd-Ctrl-T
    - [Karabiner-Elements](https://pqrs.org/osx/karabiner/)
- Download apps you want from App Store
    - Xcode
      - Once installed, open to install tools!
    - 1Password

### Let's get phys... no. Let's get automated!
- In iTerm, in the home directory, copy and paste the following:
`sh -c "$(curl -fsSL https://raw.githubusercontent.com/electricRGB/.dotfiles/master/setup.sh)"`
- Once finished running, quit terminal and reboot computer for a good time!
- Don't forget to run `p10k configure` to configure your prompt.

### Spacemacs
- Additional steps, to be done from M-x:
    - LSP: Once Spacemacs is open, run: `package-install lsp-sourcekit`
    - Also: `all-the-icons-fonts`

### Almost done!
- Run `brew doctor` to see any issues and fix them as needed
- Xcode themes from [here](https://github.com/hdoria/xcode-themes) and [here](http://www.codethemes.net/themes/popular/all) should go in `~/Library/Developer/Xcode/UserData/FontAndColorThemes/` (FontAndColorThemes may not exist.)
- [iTerm2 Themes](https://github.com/mbadolato/iTerm2-Color-Schemes)
- One last restart!

## Other Software
This is a list of the software I usually add afterwards that's not sourced from the App Store. It's here so I can just click to go there making my life easier. In no particular order

### Games
- [Tetr.io](https://tetr.io/about/desktop/)

### Communication
- [Discord](https://discord.com/new/download)
- [Skype](https://www.skype.com/en/get-skype/)

### Development
- [Arduino](https://www.arduino.cc/en/Main/Software)
- [Dash](https://kapeli.com/dash)
- [GitKraken](https://www.gitkraken.com/)

### Entertainment
- [Spotify](https://www.spotify.com/ca-en/download/mac/)

### Browsers
- [Firefox](https://www.mozilla.org/en-CA/firefox/new/)
- [Arc](https://arc.net/)

### Music
- [Band-in-a-Box](https://www.pgmusic.com/)

### Streaming
- [OBS](https://obsproject.com/download)

### Utilities
- [Amphetamine](https://apps.apple.com/us/app/amphetamine/id937984704?mt=12)
- [AppCleaner](https://freemacsoft.net/appcleaner/)
- [LoopBack](https://rogueamoeba.com/loopback/)


# Why share?
Sharing is caring. We learn from each other 🌷

# License

The contents of this repo are licensed under the [MIT license](https://opensource.org/licenses/MIT).

# Contributing

If you have any ideas or suggestions, feel free to open up an issue or shoot through a pull request! Thanks!
