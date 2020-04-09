# MY OWN CONFIGURATIONS
export PATH=$PATH:~/bin

alias e="emacs -nw"
alias desktop="pushd ~/Desktop"
alias desk='desktop'
alias downloads="pushd ~/Downloads"
alias down='downloads'
alias l='ls -alhF --color=never'
alias rm="trash" # required trash-cli

alias l32-state='VBoxManage showvminfo l32 | grep -i state'
alias l32-start='VBoxManage startvm l32 --type headless'
alias l32-poweroff='VBoxManage controlvm l32 poweroff'
alias l32-ssh='ssh -p 2222 -Y is2202@127.0.0.1'

PS1='┏━\[\e]0;\u@\h: \w\a\]${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\n┗━\$ '

bind "set completion-ignore-case on"

cd ~/Desktop

# for sdcv dictionary 
export PATH=$PATH:/usr/local/opt/sdcv/bin
export STARDICT_DATA_DIR=/usr/local/share/stardict
alias sdcv="sdcv --utf8-input --utf8-output --color"

# For some cl applications like wget
export LANG=en_US.UTF-8
