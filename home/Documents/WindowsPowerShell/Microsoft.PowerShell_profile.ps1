### Aliases
Set-alias subl 'C:\Program Files\Sublime Text 3\sublime_text.exe'
function Chezmoi-CD {
    Set-Location -Path $(chezmoi source-path)
}
Set-alias chezmoicd Chezmoi-CD

### Modules
Import-Module posh-git
. 'C:\dev\utils\Posh-Npm\profile.example.ps1'
# Import-Module pscx

### Chocolatey profile
$ChocolateyProfile = "$env:ChocolateyInstall\helpers\chocolateyProfile.psm1"
if (Test-Path($ChocolateyProfile)) {
  Import-Module "$ChocolateyProfile"
}

### GPG script
# gpg-connect-agent.exe killagent /bye
# gpg-connect-agent.exe /bye
