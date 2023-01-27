#
# Use this script as such:
#
# $linux_configuration_proj_dir = "c:\Users\canaknesil\StandalonePrograms\linux-configuration"
# . "$linux_configuration_proj_dir\powershellrc.ps1"
#
# Create HOME environment variable on Windows.
#


if ($linux_configuration_proj_dir -ne $null) {

}


#
# ALIAS AND SHORTCUTS
#

set-alias py python
set-alias ipy ipython
function gst { git status @args }
function ca { conda activate @args }
#set-alias Remove-Item trash # requires trash-cli
#set-alias rm trash
set-alias x invoke-item

function e { emacs -nw @args }
if ($IsWindows) {
    set-alias ec emacsclientw
} else {
    set-alias ec emacsclient 
}

#function matlab { matlab -nodisplay @args } # This does not work.
#function octave { octave-cli @args } # I don't have octave yet. 
#function julia { julia --color=yes @args } # Somehow does not work.

function desk { pushd "$env:HOME/Desktop" }
function down { pushd "$env:HOME/Downloads" }
function doc  { pushd "$env:HOME/Documents" }


if ($IsWindows) {
    # Bash and Zsh requires HOME environment variable.
    set-alias bash C:\msys64\usr\bin\bash.exe
}

if ($python_venv_dir -ne $null) {
    function venv-activate {
	param($env_name)
	. "$python_venv_dir\$env_name\Scripts\activate.ps1"
    }
}


#
# UTILITIES
#

function Format-HumanReadable {
    param([int64] $n)

    $units = [ordered] @{
	"T" = 1TB
	"G" = 1GB
	"M" = 1MB
	"K" = 1KB
	"" = 1
    }

    if ($n -lt 1KB) {
	"{0}" -f $n
    } else {
	foreach ($pair in $units.GetEnumerator()) {
	    if ($n -ge $pair.value) {
		return "{0:0.00}{1}" -f ($n / $pair.value), $pair.key
	    }
	}
    }
}

function Format-ChildItemHumanReadable {
    $properties = @(
	@{name="UnixMode"; expression={$_.UnixMode}; width=12}
	@{name="LastWriteTime"; expression={$_.LastWriteTime}; alignment="Right"; width=23}
	@{name="Length"
	  expression={$_.GetType() -eq [System.IO.FileInfo] ? $(Format-HumanReadable $_.Length) : ""}
	  alignment="Right"; width=12}
	@{name="Name"; expression={$_.Name}}
    )

    Get-ChildItem @args |
      Sort-Object -Property Name |
      Format-Table -Property $properties -GroupBy @{name="Directory"
						    expression={$_.PSParentPath | Convert-Path}}
}

set-alias ll Format-ChildItemHumanReadable
set-alias l  Get-ChildItem


function Get-DiskUsage {
    param(
	[parameter(valueFromPipeline)]
	[string[]] $Path = "."
    )
    process {
	foreach ($p in (get-item $Path)) {
	    $size, $dir = (du -sh $p).split()
	    [pscustomobject] @{DiskUsage=$size; Path=$(resolve-path $p -relative)}
	}
    }
}
set-alias gdu Get-DiskUsage


function Get-CrossProduct {
    [CmdletBinding()]
    param (
	[parameter(valueFromPipeline, mandatory)]
	[object[]] $Set1,
	
	[parameter(position=0, mandatory)]
	[object[]] $Set2,

	[scriptblock] $Op
    )
    process {
	foreach ($x in $set1) {
	    foreach ($y in $set2) {
		if ($Op) {
		    &($Op)
		} else {
		    $x + $y
		}
	    }
	}
    }
}
set-alias cross Get-CrossProduct


#
# SHELL CONFIGURATION
#

# For readline style line editing. 
#Import-Module PSReadLine
Set-PSReadLineOption -EditMode Emacs
# disable Predictive IntelliSense
Set-PSReadLineOption -PredictionSource None

# Prompt (posh-git module needs to be installed)
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

