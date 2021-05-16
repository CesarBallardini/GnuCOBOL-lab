#!/usr/bin/env bash

##
# nativo en MS Windows, funciona con Wine
# no compila ni corre desde dentro del editor
#
instala_notepadpp () {
  sudo snap install notepad-plus-plus

  echo "Para que funcione Notepad++ se debe correr con el siguiente mandato:"
  echo "echo 'XAUTHORITY=$HOME/.Xauthority /snap/bin/notepad-plus-plus'
  echo
}

##
# nativo en GNU/Linux
# no esta practico para compilar
#
instala_notepadqq() {
  sudo add-apt-repository ppa:notepadqq-team/notepadqq -y
  sudo apt-get update
  sudo apt-get install notepadqq
}

#instala_notepadpp
#instala_notepadqq
