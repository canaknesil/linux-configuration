# Use this script as such:

# $linux_configuration_proj_dir = "c:\Users\canaknesil\StandalonePrograms\linux-configuration"
# $linux_configuration_msys2_dir = "c:\msys64"
# $linux_configuration_python_venv_dir = "c:\Users\canaknesil\.venv"
# . "$linux_configuration_proj_dir\powershellrc.ps1"

# Windows: Create HOME environment variable.

# Windows: Create file association for .sh files to be run by Bash,
# either by "Open with..." UI, or editing the registery. This is
# required if shell scripts are desired to be run as executable.


#
# ENVIRONMENT SETUP
#

if ($linux_configuration_proj_dir -ne $null) {
    if ($IsWindows) {
	$env:Path = "${env:Path};$linux_configuration_proj_dir\bin-windows"

	# Windows doesn't have shebang (#!) to execute scripts. Only
	# files with extensions specified in PATHEXT environment
	# variable can be run as it is, given they have the necessary
	# file association. File associations can be set either via
	# the "Open with..." UI, or editing the registery.  Within
	# powershell, .ps1 scripts can also be executed by name.

	# $env:PATHEXT = "${env:PATHEXT};.SH"
    } elseif ($IsLinux) {
	# Done in $linux_configuration_proj_dir/profile
	#$env:Path = "${env:Path}:$linux_configuration_proj_dir/bin-linux"
	#$env:Path = "${env:Path}:$linux_configuration_proj_dir/bin"
    } else {
	#$env:PATH = "${env:PATH}:$linux_configuration_proj_dir/bin"
    }
}

if ($IsWindows -And ($linux_configuration_msys2_dir -ne $null)) {
    # There are a few complications of adding msys2 bin to
    # Path. Adding to the beginning makes unix commands that have a
    # different Windows implementation, such as "tree", accessible,
    # but prevents programs accessing the Windows versions. For
    # example Emacs runs properly only with Gpg4win and not with Msys2
    # version of gpg. A solution is adding msys2 bin to the end of
    # Path, and defining an alias or a function for each Msys2 command
    # that I prefer.

    # Assuming Msys2 bin directories are already to end of PATH.
    #$env:Path = "$env:Path;$linux_configuration_msys2_dir/ucrt64/bin"
    #$env:Path = "$env:Path;$linux_configuration_msys2_dir/usr/bin"

    # Bash and Zsh requires HOME environment variable.
    set-alias bash "$linux_configuration_msys2_dir/usr/bin/bash.exe"
    set-alias tree "$linux_configuration_msys2_dir/usr/bin/tree.exe"
    set-alias cat  "$linux_configuration_msys2_dir/usr/bin/cat.exe"
    set-alias find "$linux_configuration_msys2_dir/usr/bin/find.exe"
    # The existing diff alias is not writeable.
    remove-alias diff -force
    set-alias diff "$linux_configuration_msys2_dir/usr/bin/diff.exe"
}


#
# ALIAS AND SHORTCUTS
#

#set-alias Remove-Item trash # requires trash-cli
#set-alias rm trash
set-alias x invoke-item

function e { start-emacs --client @args }
function gst { git status @args }
function ca { conda activate @args }
function path {
    if ($IsWindows) {
	$env:Path -replace ";", "`n"
    } else {
	$env:PATH -replace ":", "`n"
    }
}

#function matlab { matlab -nodisplay @args } # This does not work.
#function octave { octave-cli @args } # I don't have octave yet. 
#function julia { julia --color=yes @args } # Somehow does not work.

function desk { pushd "$env:HOME/Desktop" }
function down { pushd "$env:HOME/Downloads" }
function doc  { pushd "$env:HOME/Documents" }


if ($linux_configuration_python_venv_dir -ne $null) {
    function venv-activate {
	param($env_name)
	. "$linux_configuration_python_venv_dir\$env_name\Scripts\activate.ps1"
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
#Import-Module PSReadLine # Enabled by default
Set-PSReadLineOption -EditMode Emacs
# disable Predictive IntelliSense
Set-PSReadLineOption -PredictionSource None

# Prompt (posh-git module needs to be installed)
# PowerShellGet\Install-Module posh-git -Scope CurrentUser -Force
# PowerShellGet\Update-Module posh-git
function prompt {
    $debug = $(if (Test-Path variable:/PSDebugContext) { '[DBG] ' } else { '' })
    $cwd = $(get-location)
    if (-not $IsWindows) {
	$cwd = $cwd -replace "^${env:HOME}", '~'
    }
    if ($IsWindows) {
	$user = $env:USERNAME
    } else {
	$user = $env:USER
    }
    $hostname_str = [System.Net.Dns]::GetHostname()
    $last_char = $(if ($NestedPromptLevel -ge 1) { '>>' }) + '> '

    if (get-command get-gitdirectory -ErrorAction SilentlyContinue) {
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
    }
    
    "${debug}PS `e[1;33m${user}@${hostname_str}`e[0m `e[1;36m${cwd}`e[1;33m`e[0m`e[1;32m${git_prompt_str}`e[0m`n${last_char}`e[0m"
}
# For colors: https://en.wikipedia.org/wiki/ANSI_escape_code


echo "Executed Linux Configuration profile."
if ($linux_configuration_proj_dir -ne $null) {
    echo "$linux_configuration_proj_dir/powershellrc.ps1"
}

