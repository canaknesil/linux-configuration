#
# Use this script as such:
#
# LINUX_CONFIGURATION_PROJ_DIR=~/seperate-programs/linux-configuration
# PYTHON_VENV_DIR=~/.venv
# . $LINUX_CONFIGURATION_PROJ_DIR/profile
#
# Used in combination with $LINUX_CONFIGURATION_PROJ_DIR/bashrc
#                       or $LINUX_CONFIGURATION_PROJ_DIR/powershellrc.ps1
#
# Use "profile" rather than "bashrc", or similar, whenever possible
# so that programs started outside shell can have the configuration.
#


if [ -n "$LINUX_CONFIGURATION_PROJ_DIR" ]; then
    export PATH=$PATH:$LINUX_CONFIGURATION_PROJ_DIR/bin
    export PATH=$PATH:$LINUX_CONFIGURATION_PROJ_DIR/bin-linux
fi


if [ -n "$PYTHON_VENV_DIR" ]; then
    export WORKON_HOME=$PYTHON_VENV_DIR # For emacs elpy pyvenv-* commands.
fi



