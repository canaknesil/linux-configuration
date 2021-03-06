#
# Use this script as such:
#
# LINUX_CONFIGURATION_PROJ_DIR=~/seperate-programs/linux-configuration
# PYTHON_VENV_DIR=~/.venv
# . $LINUX_CONFIGURATION_PROJ_DIR/profile
#

export PATH=$PATH:~/bin
if [ -n "$LINUX_CONFIGURATION_PROJ_DIR" ]; then
    export PATH=$PATH:$LINUX_CONFIGURATION_PROJ_DIR/bin
fi


if [ -n "$PYTHON_VENV_DIR" ]; then
    export WORKON_HOME=$PYTHON_VENV_DIR # For emacs elpy pyvenv-* commands.
    venv-activate() {
	. $PYTHON_VENV_DIR/$1/bin/activate
    }
fi



