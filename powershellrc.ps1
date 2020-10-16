#
# Use this script as such:
#
# $linux_configuration_proj_dir = "c:\Users\canaknesil\StandalonePrograms\linux-configuration"
# $python_venv_dir = "c:\Users\canaknesil\.venv"
# . "$linux_configuration_proj_dir\powershellrc.ps1"
#

if ($IsWindows) {
    $homedir = $env:USERPROFILE
} else {
    $homedir = $env:HOME
}

if ($linux_configuration_proj_dir -ne $null) {
    #export PATH=$PATH:$LINUX_CONFIGURATION_PROJ_DIR/bin
}

set-alias l get-childitem
set-alias py python
set-alias ipy ipython
function gst { git status @args }

function e { emacs -nw @args }
if ($IsWindows) {
    function ec { emacsclientw --server-file "$env:UserProfile\.emacs.d\server\server" @args }
    # Start emacs server with ".../runemacs.exe --daemon".
    # Put the shortcut to "C:\Users\canaknesil\AppData\Roaming\Microsoft\Windows\Start Menu\Programs\Startup". It did not work.
} else {
    set-alias ec emacsclient
}

#function matlab { matlab -nodisplay @args } # This does not work.
#function octave { octave-cli @args } # I don't have octave yet. 
#function julia { julia --color=yes @args } # Somehow does not work.

if ($IsWindows) {
    function desk { pushd "c:\Users\$env:USERNAME\Desktop" }
    function down { pushd "d:\Users\$env:USERNAME\Downloads" }
    function doc  { pushd "d:\Users\$env:USERNAME\Documents" }
    function homec { pushd "c:\Users\$env:USERNAME" }
    function homed { pushd "d:\Users\$env:USERNAME" }
} else {
    function desk { pushd "$homedir/Desktop" }
    function down { pushd "$homedir/Downloads" }
    function doc  { pushd "$homedir/Documents" }
}

# Bash and Zsh for windows
if ($IsWindows) {
    $env:HOME = $env:USERPROFILE # Could not find a way to run only command with an environment variable.
    set-alias bash C:\msys64\usr\bin\bash.exe
}

function Get-DiskUsage {
    param(
	[parameter(valueFromPipeline)]
	[string[]] $path
    )
    process {du -hs $path}
}
set-alias gdu Get-DiskUsage

if ($python_venv_dir -ne $null) {
    function venv-activate {
	param($env_name)
	. "$python_venv_dir\$env_name\Scripts\activate.ps1"
    }
}

# For readline style line editing. 
#Import-Module PSReadLine
Set-PSReadLineOption -EditMode Emacs
