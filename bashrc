#
# Use this script as such:
#
# LINUX_CONFIGURATION_PROJ_DIR=~/seperate-programs/linux-configuration
# . $LINUX_CONFIGURATION_PROJ_DIR/bashrc
#

alias e="emacs -nw"
alias matlab='matlab -nodisplay'
alias octave="octave-cli"
alias julia='julia --color=yes'
alias sdcv="sdcv --utf8-input --utf8-output --color"
alias l='ls -alhF' # --color=never doen't work on mac.
# alias rm="trash" # requires trash-cli
alias py='python3'
alias ipy='ipython3'

alias desktop="pushd ~/Desktop"
alias desk='desktop'
alias downloads="pushd ~/Downloads"
alias down='downloads'

# Ignore case during completion on bash
if [ -n "$BASH_VERSION" ]; then
  bind "set completion-ignore-case on"
fi
