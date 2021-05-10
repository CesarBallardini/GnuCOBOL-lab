# README - GnuCOBOL Lab

Aprende GnuCOBOL en este laboratorio.

Usaremos una máquina virtual (VM) con Ubuntu 20.04 y GnuCOBOL 3.1.2.

La VM se crea mediante Vagrant sobre Virtualbox.  Luego se la aprovisiona con Ansible para que
disponga de un entorno completo para desarrollar aplicaciones con GnuCOBOL.

Daremos algunos tips para compilar código RM COBOL 85.

Seguiremos los ejemplos y pasos del libro "Programación estructurada en RM/COBOL 85", de Fernando García Pérez,
Almudena Cerro Somolino y José Manuel Diez PErla; Mc Graw-Hill, 1989.

Cada capítulo del libro se estudiará en un directorio específico.

# Requisitos para usar este repositorio

* Virtualbox
* Vagrant + plugins
* Ansible
* Git

# Cómo usar este repositorio

Luego de instalar los requisitos, en una terminal ejecuta:

```bash
git clone https://github.com/CesarBallardini/GnuCOBOL-lab
cd GnuCOBOL-lab/
ansible-galaxy install -r requirements.yml --roles-path=provision/ansible/roles/ --force

vagrant up     # enciende, actualiza y hace la provisión del software solicitado
vagrant reload # para hacer efectiva la actualización del sistema que se realizó
```


Esto tardará unos minutos, tiempo que depende de tu hardware y de la velocidad de tu conexión a la Internet.

Cuando este proceso termine, tendrás una VM con todo lo necesario para tu estudio de GnuCOBOL.

Puedes ingresar a esa VM si escribes:


```bash
vagrant ssh
```

Eso te dará una terminal de texto con un shell de la VM y te muestra:

```text
vagrant@coboldev:~$ _
```


Ahora puedes pasar al primer tema, en: [el laboratorio de COBOL](penrmc85/README.md)

Cuando deseas apagar la VM, corre:

```bash
vagrant halt
```

Cuando la enciendas con `vagrant up` nnuevamente, todo estará como lo dejaste.

Y cuando no la necesites más puedes eliminarla con:

```bash
vagrant destroy
```

Todo lo que dejes en el directorio `/vagrant/` quedará en tu pc host, lo que dejes en tu HOME (`/home/vagrant/`) se perderá cuando destruyas la VM.


# Referencias

* https://es.wikipedia.org/wiki/GnuCOBOL
* https://en.wikipedia.org/wiki/GnuCOBOL
* https://ftp.gnu.org/gnu/gnucobol/ Descargas oficiales desde el proyecto GNU
* https://sourceforge.net/projects/gnucobol/ proyecto en SourceForge
  * https://sourceforge.net/projects/gnucobol/files/ descargas
  * https://sourceforge.net/p/gnucobol/code/HEAD/tree/ código en Subversion
  * `svn checkout https://svn.code.sf.net/p/gnucobol/code/trunk gnucobol-code` obtener la versión de `trunk`
  * https://gnucobol.sourceforge.io/guides.html GnuCOBOL Guides
  * https://gnucobol.sourceforge.io/faq/ GnuCOBOL FAQ navegable
  * https://gnucobol.sourceforge.io/faq/GnuCOBOLFAQ.pdf GnuCOBOL FAQ en PDF
  * https://sourceforge.net/p/gnucobol/code/HEAD/tree/external-doc/guide/PDFs/gnucobpg-a4.pdf?format=raw GnuCOBOL 3.1 (dev) Programmers Guide (A4).pdf
  * https://sourceforge.net/p/gnucobol/code/HEAD/tree/external-doc/guide/PDFs/gnucobqr-a4.pdf?format=raw GnuCOBOL 3.1 Quick Reference (A4).pdf
  * https://sourceforge.net/p/gnucobol/code/HEAD/tree/external-doc/guide/PDFs/gnucobsp-a4.pdf?format=raw GnuCOBOL 3.1 Sample Programs (A4).pdf

