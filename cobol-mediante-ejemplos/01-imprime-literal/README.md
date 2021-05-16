# Imprime un literal en pantalla

Vamos a mostrar los tipos de datos elementales que existen en COBOL, en la pantalla.

* números enteros
* números con coma decimal
* cadenas de caracteres

El más simple de todos los programas es el "Hello, World!", que sólo muestra una cadena de caracteres y termina:

[hello.cbl](01-imprime-literal/hello.cbl)

# ¿Qué tenemos aquí?

1. Las partes del programa COBOL
2. La división en columnas de las líneas de programa

```cobol
000100* HELLO.CBL
000200 IDENTIFICATION DIVISION.
000300 PROGRAM-ID. hello.
000400 PROCEDURE DIVISION.
000500 SAY-HELLO.
000600     DISPLAY "Hello, world".
000700     STOP RUN.
```

Las primeras 6 columnas son para el número de página y de línea del programa, esto viene desde la época de las hojas de codificación COBOL.  Es opcional.

La columna 7 tiene un espacio en blanco si es una línea normal de programa, o un `*` si es una línea de comentario.

El mínimo programa tiene la `IDENTIFICATION DIVISION` donde se le da el nombre con `PROGRAM-ID` y la `PROCEDURE DIVISION` donde van las instrucciones.

El código del programa va en el margen A (columna 8 á 11) o en el margen B (columna 12 á 72).

Los nombres de división y sección, y los nombres de párrafo, deben comenzar en el margen A.  Las instrucciones dentro del párrafo van en el margen B.

`SAY-HELLO` es un nombre de párrafo.

`DISPLAY` muestra en pantalla.  `STOP RUN` detiene la ejecución del programa.


# Compilamos el programa y lo ejecutamos

Hay varias opciones con GnuCOBOL para ejecutar un programa.

* **binario ejecutable:** GnuCOBOL genera un archivo ejecutable

* **módulo:** GnuCOBOL permite compilar varios módulos y ejecutar con un runtime el resultado de la compilación.

* **script:** si el programa es pequeño y no necesita nada fuera de sí, podemos incorporar una línea en los sistemas GNU/Linux para que
se lo considere interpretado, y se llame a su intérprete.  En el caso de GnuCOBOL, esa línea invocará al compilador y ejecutará el resultado de 
la compilación, todo en la misma operación.

##  Binario ejecutable

Nos movemos al directorio donde está el fuente y lo compilamos:

```bash
cd /vagrant/cobol-mediante-ejemplos/01-imprime-literal/
cobc -x -Wall hello.cbl
```

Eso genera un archivo `hello`  que podemos analizar con el mandato `file hello`:

```text
hello: ELF 64-bit LSB shared object, x86-64, version 1 (SYSV), dynamically linked, interpreter /lib64/ld-linux-x86-64.so.2, for GNU/Linux 3.2.0, BuildID[sha1]=855c6ece50a5fce39e64d084156d72758df5c0f1, not stripped

```

O sea, es un ejecutable dinámicamente enlazado (requiere las libs de GnuCOBOL para funcionar).

Lo ejecutamos con `hello` y obtenemos:

```text
Hello, world
```


## Como módulos dinámicos

```bash
cd /vagrant/cobol-mediante-ejemplos/01-imprime-literal/
cobc -m hello.cbl
cobcrun hello
```

En este caso, la salida de `file hello.so` muestra:

```text
hello.so:  ELF 64-bit LSB shared object, x86-64, version 1 (SYSV), dynamically linked, BuildID[sha1]=08e080a264783b4677b9db5c869aa7f1f88cc4cc, not stripped
```

En este caso, al igual que con el ejecutable de enlace dinámico, hace falta la biblioteca `libcob.so.4 => /usr/local/lib/libcob.so.4` que sale de la instalación de GnuCOBOL.


