#
# Use this script as such:
#
# $linux_configuration_proj_dir = "c:\Users\canaknesil\StandalonePrograms\linux-configuration"
# $python_venv_dir = "c:\Users\canaknesil\.venv"
# . "$linux_configuration_proj_dir\powershellrc.ps1"
#

if ($linux_configuration_proj_dir -ne $null) {
    #export PATH=$PATH:$LINUX_CONFIGURATION_PROJ_DIR/bin
}

function e { emacs -nw @args } # The theme is horrible. 
function ec { emacsclientw --server-file "$env:UserProfile\.emacs.d\server\server" @args }
# Start emacs server with ".../runemacs.exe --daemon".
# Put the shortcut to "C:\Users\canaknesil\AppData\Roaming\Microsoft\Windows\Start Menu\Programs\Startup".

#function matlab { matlab -nodisplay @args } # This does not work.
#function octave { octave-cli @args } # I don't have octave yet. 
#function julia { julia --color=yes @args } # Somehow does not work.
function l { ls @args }
function py { python @args }
function ipy { ipython @args }
# Can't find anythink to send files to trash.

function desk { pushd "c:\Users\$env:USERNAME\Desktop" }
function down { pushd "d:\Users\$env:USERNAME\Downloads" }
function homec { pushd "c:\Users\$env:USERNAME" }
function homed { pushd "d:\Users\$env:USERNAME" }

# Rather than creating links (sometimes they don't work...) creating functions.
function bash { C:\msys64\usr\bin\bash.exe --rcfile "$env:USERPROFILE\.native_bashrc" @args }
function du {
    param(
	[parameter(valueFromPipeline)]
	$path
    )
    process {C:\msys64\usr\bin\du.exe -hs $path}
}

if ($python_venv_dir -ne $null) {
    function venv-activate {
	param($env_name)
	. "$python_venv_dir\$env_name\Scripts\activate.ps1"
    }
}

# For readline style line editing. 
Import-Module PSReadLine
Set-PSReadLineOption -EditMode Emacs
