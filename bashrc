
# MY OWN CONFIGURATIONS
export PATH=$PATH:~/bin

alias e="emacs -nw"
alias desktop="pushd ~/Desktop"
alias desk='desktop'
alias downloads="pushd ~/Downloads"
alias down='downloads'

# for sdcv dictionary 
export PATH=$PATH:/usr/local/opt/sdcv/bin
export STARDICT_DATA_DIR=/usr/local/share/stardict
alias sdcv="sdcv --utf8-input --utf8-output --color"

# For some cl applications like wget
export LANG=en_US.UTF-8
