[ -f ~/.fzf.bash ] && source ~/.fzf.bash
# Sometimes this gets loaded instead of .bash_profile
# This ensures we use the correct file
[ -n "$PS1" ] && source ~/.bash_profile;
