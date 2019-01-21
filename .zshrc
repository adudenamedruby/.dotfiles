## If you come from bash you might have to change your $PATH.
export PATH="/usr/local/sbin:$PATH"

# Path to your oh-my-zsh installation, depending on user
CURRENTUSER="$(id -un)"
export ZSH=/Users/$CURRENTUSER/.oh-my-zsh
unset $CURRENTUSER

# Set name of the theme to load. Optionally, if you set this to "random"
# it'll load a random theme each time that oh-my-zsh is loaded.
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
ZSH_THEME="powerlevel9k/powerlevel9k"

#POWERLEVEL9k customization
POWERLEVEL9K_MODE="nerdfont-complete"
POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(os_icon context dir_writable dir vcs newline time root_indicator command_execution_time status)
POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=()
POWERLEVEL9K_USER_ICON="\uF415" # 
POWERLEVEL9K_ROOT_ICON="\uF09C"
POWERLEVEL9K_SUDO_ICON=$'\uF09C' # 
POWERLEVEL9K_TIME_FORMAT="%D{%H:%M:%S}"
POWERLEVEL9K_VCS_GIT_ICON='\uF408 '
POWERLEVEL9K_VCS_GIT_GITHUB_ICON='\uF408 '

POWERLEVEL9K_CONTEXT_TEMPLATE="\uF415 %n@%m"
POWERLEVEL9K_CONTEXT_DEFAULT_BACKGROUND="white"
POWERLEVEL9K_CONTEXT_DEFAULT_FOREGROUND="0"
POWERLEVEL9K_CONTEXT_SUDO_BACKGROUND="9"
POWERLEVEL9K_CONTEXT_SUDO_FOREGROUND="7"
POWERLEVEL9K_CONTEXT_ROOT_FOREGROUND="12"
POWERLEVEL9K_CONTEXT_ROOT_FOREGROUND="7"
POWERLEVEL9K_CONTEXT_REMOTE_FOREGROUND="10"
POWERLEVEL9K_CONTEXT_REMOTE_FOREGROUND="7"
POWERLEVEL9K_CONTEXT_REMOTE_SUDO_FOREGROUND="1"
POWERLEVEL9K_CONTEXT_REMOTE_SUDO_FOREGROUND="7"

POWERLEVEL9K_HOME_ICON='\uF015'
POWERLEVEL9K_HOME_SUB_ICON='\uFC6E'
POWERLEVEL9K_FOLDER_ICON='\uF74E'
POWERLEVEL9K_ETC_ICON='\uFB1A'
POWERLEVEL9K_DIR_SHOW_WRITABLE=true

POWERLEVEL9K_TIME_BACKGROUND="yellow"
POWERLEVEL9K_TIME_FOREGROUND="black"

P9K_VCS_CLEAN_BACKGROUND="green"
P9K_VCS_CLEAN_FOREGROUND="$DEFAULT_BACKGROUND"
P9K_VCS_MODIFIED_BACKGROUND="yellow"
P9K_VCS_MODIFIED_FOREGROUND="$DEFAULT_BACKGROUND"
P9K_VCS_UNTRACKED_BACKGROUND="magenta"
P9K_VCS_UNTRACKED_FOREGROUND="$DEFAULT_BACKGROUND"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion. Case
# sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(
    git
    colored-man-pages
    vi-mode
    zsh-syntax-highlighting
)

source $ZSH/oh-my-zsh.sh

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/rsa_id"

# Example aliases
[ -f ~/.aliases ] && source ~/.aliases
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
