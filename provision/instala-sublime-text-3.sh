#!/usr/bin/env bash


instala_sublime() {

  sudo apt-get install libgtk3.0 -y
  sudo snap install sublime-text --classic
}


elimina_personalizaciones() {

  rm -rf ~/.config/sublime-text-3/ ~/snap/sublime-text/

}

instala_package_control() {

  sublime-text.subl --version > /dev/null  # crea los directorios personales
  mkdir -p ~/.config/sublime-text-3/Installed\ Packages ~/.config/sublime-text-3/Packages/User
  curl http://sublime.wbond.net/Package%20Control.sublime-package > ~/.config/sublime-text-3/Installed\ Packages/Package\ Control.sublime-package
  sublime-text.subl --version > /dev/null  # instala package control

}

instala_plugins() {


  cat <<EOF > ~/.config/sublime-text-3/Packages/User/Package\ Control.sublime-settings
{
	"bootstrapped": true,
	"in_process_packages":
	[
	],
	"installed_packages":
	[
		"BlackBird-ColorScheme",
		"COBOL Syntax",
		"Package Control",
		"Terminus"

	]
}
EOF

  sublime-text.subl --version > /dev/null  # instala plugins
}


##
# main
#

elimina_personalizaciones
instala_sublime
instala_package_control
instala_plugins


##
# Referencias
#

# Package Control
#   https://packagecontrol.io/installation
#   https://github.com/wbond/package_control

# BlackBird-ColorScheme
#   https://packagecontrol.io/packages/BlackBird-ColorScheme
#   https://github.com/jonatasnardi/BlackBird-ColorScheme

# Cobol Syntax
#   https://packagecontrol.io/packages/COBOL%20Syntax
#   https://bitbucket.org/bitlang/sublime_cobol/src/master/

# Terminus
#   https://packagecontrol.io/packages/Terminus
#   https://github.com/randy3k/Terminus
