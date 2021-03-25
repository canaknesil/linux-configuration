#
# Use this script as such:
#
# $linux_configuration_proj_dir = "c:\Users\canaknesil\StandalonePrograms\linux-configuration"
# $python_venv_dir = "c:\Users\canaknesil\.venv"
# . "$linux_configuration_proj_dir\powershellrc.ps1"
#
# Create HOME environment variable on Windows. This will be used by bash, zsh, and emacs. 
#

if ($linux_configuration_proj_dir -ne $null) {
    #export PATH=$PATH:$LINUX_CONFIGURATION_PROJ_DIR/bin
}

set-alias l get-childitem
set-alias py python
set-alias ipy ipython
function gst { git status @args }

function e { emacs -nw @args }
if ($IsWindows) {
    set-alias ec emacsclientw
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
    function desk { pushd "$env:HOME/Desktop" }
    function down { pushd "$env:HOME/Downloads" }
    function doc  { pushd "$env:HOME/Documents" }
}

if ($IsWindows) {
    # Bash and Zsh required HOME environment variable.
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

# Prompt
import-module posh-git
$GitPromptSettings.DefaultPromptAbbreviateHomeDirectory = $true

$GitPromptSettings.DefaultPromptPrefix = "PS "
$GitPromptSettings.DefaultPromptSuffix = '`n$(">" * ($nestedPromptLevel + 1)) '
if ($IsWindows) {
    $GitPromptSettings.DefaultPromptDebugSuffix = '`n[DBG]$(">" * ($nestedPromptLevel + 1)) ' # Does not exist on Linux.
    $GitPromptSettings.BranchIdenticalStatusToSymbol = ""
    $GitPromptSettings.LocalStagedStatusForegroundColor = "Green"
} else {
    $GitPromptSettings.DefaultPromptDebug.Text = '`n[DBG]$(">" * ($nestedPromptLevel + 1)) ' # Does not exist on Linux.
    $GitPromptSettings.BranchIdenticalStatusSymbol.Text = ""
    $GitPromptSettings.LocalStagedStatusSymbol.ForegroundColor = "Green"
}
#$GitPromptSettings.DefaultForegroundColor = 'Yellow'
$GitPromptSettings.ShowStatusWhenZero = $false
$GitPromptSettings.LocalWorkingStatusSymbol = "*"
