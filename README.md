# README - GnuCOBOL Lab

Aprende GnuCOBOL en este laboratorio.

Usaremos una máquina virtual (VM) con Ubuntu 20.04 y GnuCOBOL 3.1.2.

La VM se crea mediante Vagrant sobre Virtualbox.  Luego se la aprovisiona con Ansible para que
disponga de un entorno completo para desarrollar aplicaciones con GnuCOBOL.

Daremos algunos tips para compilar código RM COBOL 85.

Seguiremos los ejemplos y pasos del libro "Programación estructurada en RM/COBOL-85", de Fernando García Pérez,
Almudena Cerro Somolino y José Manuel Diez Perla; McGraw-Hill, 1990.

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

Tienes disponible el IDE Sublime Text 3, mediante el mandato: `subl`.


Ahora puedes pasar al primer tema, en: [el laboratorio de COBOL](penrmc85/README.md).

Si prefieres aprender practicando con programas, prueba [COBOL mediante ejemplos](cobol-mediante-ejemplos/README.md).


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

# OpenCobol IDE

Hasta 2017 estuvo en desarrollo y mantenimiento esta IDE.  En este momento está abandonada.

En las versiones más modernas de las distribuciones no funciona correctamente, pues ya no
se satisfacen sus dependencias.

Hemos hecho tres programas auxiliares para su instalación, y en la siguiente tabla 
se puede apreciar en cuál distro funciona cada uno.

* `pip_install_36`: instala mediante `pip` con `python3.6`
* `pip_install_37`: instala mediante `pip` con `python3.7`
* `deb_install`: utiliza los paquetes DEB del sistema operativo.


|                     | `pip_install_36` | `pip_install_37` | `deb_install`|
|---------------------|------------------|------------------|--------------|
| Ubuntu 20.04 focal  |       no         |       no         |      no      |
| Ubuntu 18.04 bionic |       si         |       si         |      si      |
| Ubuntu 16.04 xenial |       no         |       no         |      si      |


Se puede apreciar que ninguna de las alternativas sirve para instalar OpenCobol IDE sobre Ubuntu 20.04.

La instalación mediante paquetes DEB es apropiada en las versiones 18.04 y 16.04 de Ubuntu.  

Ubuntu 16.04 (Xenial) se verificó tanto en 64 bits como en 32 bits.  Esta distro no dispone de `snap` y por lo tanto no le hemos instalado SublimeText3.


Una opción que tiene ambos editores es Ubuntu 18.04 Bionic.  Tanto OpenCobol IDE como SublimeText3 pueden instalarse en esa distro.  Bionic sólo
está disponible en la arquitectura de 64 bits.


El cambio de sistema operativo en la VM se hace modificando en el archivo `Vagrantfile` y descomentando una sola
de las líneas que dicen:

```ruby
    #srv.vm.box = "ubuntu/focal64"
    srv.vm.box = "ubuntu/bionic64"
    #srv.vm.box = "ubuntu/xenial64"
    #srv.vm.box = "ubuntu/xenial32"
```

La VM de 32 bits puede correr con mejor performance en computadores más antiguos.


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

* https://riptutorial.com/Download/cobol.pdf Tutorial COBOL Creative Commons BY-SA
* https://tug.org/TUGboat/tb10-4/tb26mcclure.pdf Diagramas de sintaxis COBOL en TeX
* https://gnucobol.sourceforge.io/faq/index.html#bubble-cobol-tcl diagramas de burbujas para la sintaxis de GnuCOBOL, escrito en Tcl
* https://github.com/petli/cobol-sharp This is a tool to extract code structure from COBOL written according to mid-80's best practices and revisualize it as more modern code structures. The purpose is to make it easier to analyze legacy code to understand what it does, extract the core business logic, and then reimplement it in modern languages.
