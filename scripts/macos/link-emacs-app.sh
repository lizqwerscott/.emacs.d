sudo rm -rf /Applications/Emacs.app &&
osascript -e 'tell application "Finder" to make alias file to posix file "/opt/homebrew/opt/emacs-plus@31/Emacs.app" at posix file "/Applications" with properties {name:"Emacs.app"}'
