#!/usr/bin/env bash

# el instalador pip funciona sólo hasta python3.7
# https://stackoverflow.com/questions/58758447/how-to-fix-module-platform-has-no-attribute-linux-distribution-when-instal el mismo problema en pylint 2019
# https://bugs.python.org/issue28167 platform.linux_distribution() was deprecated in 3.5, and should be removed.
# Ubuntu 20.04 no tiene python3.7

pip_install_36() { # Ubuntu 20.04: no / Ubuntu 18.04: si / Ubuntu 16.04: no

  sudo apt-get install python3.6 python3.6-dev python3-pip libgl1-mesa-dev -y
  sudo python3 -m pip install pip --upgrade
  sudo python3.6 -m pip install pip --upgrade

  sudo python3 -m pip install PyQt5 --upgrade
  sudo python3 -m pip install wheel setuptools testresources --upgrade
  sudo python3 -m pip install aenum
  sudo python3 -m pip install OpenCobolIDE --upgrade
} 

pip_install_37() { # Ubuntu 20.04: no / Ubuntu 18.04: si / Ubuntu 16.04: no

  sudo apt-get install python3.7 python3.7-dev python3-pip libgl1-mesa-dev -y
  sudo python3.7 -m pip install pip --upgrade

  sudo python3.7 -m pip install PyQt5 --upgrade
  sudo python3.7 -m pip install wheel setuptools testresources --upgrade
  sudo python3.7 -m pip install aenum
  sudo python3.7 -m pip install OpenCobolIDE --upgrade
} 

deb_install() { # Ubuntu 20.04: no / Ubuntu 18.04: si / Ubuntu 16.04: si

  # open-cobol es una dependencia de python3-opencobolide, aunque se usara el GnuCOBOL seleccionado con Ansible
  sudo apt-get install open-cobol python3-pyqt5 xdg-utils -y 
  sudo apt --fix-broken install -y
  wget https://github.com/OpenCobolIDE/OpenCobolIDE/releases/download/4.7.6/python3-opencobolide_4.7.6-1_all.deb
  sudo dpkg -i python3-opencobolide_4.7.6-1_all.deb

  # workaround: https://stackoverflow.com/questions/47878060/why-is-the-re-module-trying-to-import-enum-intflag
  sudo rm -f /usr/lib/python3/dist-packages/open_cobol_ide/extlibs/enum.py

}


##
# main
#

if [ "$(lsb_release  -cs)" = "xenial" -o "$(lsb_release  -cs)" = "bionic" ]
then
  deb_install
fi



# https://launchpad.net/cobcide A simple OpenCobol IDE
# https://github.com/OpenCobolIDE/OpenCobolIDE
# https://github.com/OpenCobolIDE/OpenCobolIDE/issues/439 OCIDE is not maintained anymore #439 - 15 Oct 2017

# https://github.com/HackEdit/hackedit/ era el sucesor de OpenCobolIDE pero no está más

# sudo apt-get install geany -y # IDE simplista donde se ve bien el COBOL
