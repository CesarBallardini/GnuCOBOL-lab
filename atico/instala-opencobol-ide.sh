#!/usr/bin/env bash

# el instalador pip funciona sólo hasta python3.7
# https://stackoverflow.com/questions/58758447/how-to-fix-module-platform-has-no-attribute-linux-distribution-when-instal el mismo problema en pylint 2019
# https://bugs.python.org/issue28167 platform.linux_distribution() was deprecated in 3.5, and should be removed.
# Ubuntu 20.04 no tiene python3.7

pip_install() { # no funciona en Ubuntu 20.04
  sudo apt-get install python3-pip python3-dev libgl1-mesa-dev -y
  sudo python3 -m pip install pip --upgrade

  sudo python3 -m pip install PyQt5 --upgrade
  sudo python3 -m pip install wheel setuptools testresources --upgrade
  sudo python3 -m pip install OpenCobolIDE --upgrade
} 

deb_install() { # no funciona en Ubuntu 20.04
  sudo apt-get install open-cobol python3-pyqt5 -y 
  sudo apt --fix-broken install -y
  wget https://github.com/OpenCobolIDE/OpenCobolIDE/releases/download/4.7.6/python3-opencobolide_4.7.6-1_all.deb
  sudo dpkg -i python3-opencobolide_4.7.6-1_all.deb
}


# https://launchpad.net/cobcide A simple OpenCobol IDE
# https://github.com/OpenCobolIDE/OpenCobolIDE
# https://github.com/OpenCobolIDE/OpenCobolIDE/issues/439 OCIDE is not maintained anymore #439 - 15 Oct 2017

# https://github.com/HackEdit/hackedit/ era el sucesor de OpenCobolIDE pero no está más

# sudo apt-get install geany -y # IDE simplista donde se ve bien el COBOL
