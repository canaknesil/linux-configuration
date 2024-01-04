#
# Use this script as such:
#
# LINUX_CONFIGURATION_PROJ_DIR=~/seperate-programs/linux-configuration
# . $LINUX_CONFIGURATION_PROJ_DIR/bashrc
#
# Used in combination with $LINUX_CONFIGURATION_PROJ_DIR/profile
#
# Use "profile" rather than "bashrc" whenever possible so that
# programs started outside shell can have the configuration.
#

alias e="start-emacs"
alias matlab='matlab -nodisplay'
alias octave="octave-cli"
alias julia='julia --color=yes'
alias sdcv="sdcv --utf8-input --utf8-output --color"
alias l='ls -alhF' # --color=never doen't work on mac.
# alias rm="trash" # requires trash-cli
alias ca='conda activate'
alias gst='git status'

alias desk="pushd ~/Desktop"
alias down="pushd ~/Downloads"
alias doc='pushd ~/Documents'

h () {
  history | grep $@
}

if [ -n "$PYTHON_VENV_DIR" ]; then
    venv-activate() {
	. $PYTHON_VENV_DIR/$1/bin/activate
    }
fi

# TODO: test this function
# run () {
#   $@ & disown $1
# }

# Ignore case during completion on bash
if [ -n "$BASH_VERSION" ]; then
  bind "set completion-ignore-case on"
fi
