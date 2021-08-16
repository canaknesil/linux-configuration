#
# Use this script as such:
#
# $linux_configuration_proj_dir = "c:\Users\canaknesil\StandalonePrograms\linux-configuration"
# . "$linux_configuration_proj_dir\powershellrc.ps1"
#
# Create HOME environment variable on Windows. This will be used by bash, zsh, and emacs. 
#

# TODO
# Zshell style completion (matching middle of words etc.)
# Revert to Bash completion (ex: systemctl status TAB)
# Powershell interface for command Linux programs (ex: df, systemctl, iptables, etc.)

if ($linux_configuration_proj_dir -ne $null) {

}

set-alias l get-childitem
set-alias py python
set-alias ipy ipython
function gst { git status @args }
function ca { conda activate @args }
#set-alias Remove-Item trash # requires trash-cli
#set-alias rm trash

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
    # Bash and Zsh requires HOME environment variable.
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
function prompt {
    $debug = $(if (Test-Path variable:/PSDebugContext) { '[DBG] ' } else { '' })
    $cwd = $(get-location)
    if (-not $IsWindows) {
	$cwd = $cwd -replace "^${env:HOME}", '~'
    }
    $user = $env:USERNAME
    $hostname_str = [System.Net.Dns]::GetHostname()
    $last_char = $(if ($NestedPromptLevel -ge 1) { '>>' }) + '> '

    $is_git = $(get-gitdirectory) -ne $null
    if ($is_git) {
	$git_status = (get-gitstatus)

	$git_prompt_str = ' '
	$git_prompt_str += $git_status.branch + ' '
	if ($git_status.hasworking) {$git_prompt_str += '*'}
	if ($git_status.hasindex) {$git_prompt_str += '+'}
	if ($git_status.AheadBy -gt 0) {$git_prompt_str += '↑'}
	if ($git_status.BehindBy -gt 0) {$git_prompt_str += '↓'}
    }
    
    "${debug}PS `e[33m${user}@${hostname_str}`e[0m `e[94m${cwd}`e[0m`e[32m${git_prompt_str}`e[0m`n`e[90m${last_char}`e[0m"
}
# For colors: https://en.wikipedia.org/wiki/ANSI_escape_code

