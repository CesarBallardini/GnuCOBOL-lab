# Capítulo 3 - Introducción a los lenguajes de programación.  Lenguaje COBOL.

## 3.1. Introducción a los lenguajes de programación. Lenguaje COBOL

El lenguaje COBOL es un lenguaje de alto nivel, orientado a la gestión comercial
y de tipo compilado.  Sus instrucciones se codifican utilizando reglas semejantes a la construcción de frases en inglés.


## 3.1. Historia y características del lenguaje COBOL

TODO

## 3.2. Estructura de un programa COBOL

Todo programa COBOl está compuesto por cuatro grandes partes, denominadas **divisiones**.
Las divisiones se deben escribir todas, y en el orden especificado:

* `IDENTIFICATION DIVISION`: identifica al programa con su nombre, autor, fecha y restricciones de uso.
* `ENVIRONMENT DIVISION`: especifica las características del computador y los periféricos usados.
* `DATA DIVISION`: se definen los datos usados por el programa, indicando en cada caso el nombre, tipo y tamaño.
* `PRECEDURE DIVISION`: aquí están las instrucciones que se ejecutarán en el programa.


Estructura general de un fuente COBOL:

```text
IDENTIICATION DIVISION.

PROGRAM ID. nombre-del-programa
[AUTHOR. [comentario] ... ]
[INSTALLATION. [comentario] ...]
[DATE-WRITTEN. [comentario] ...]
SECURITY. [ comentario] ...]


ENVIRONMENT DIVISION.

CONFIGURATION SECTION.
SOURCE-COMPUTER. nombre-de-computador.
OBJECT-COMPUTER. nombre-de-computador.
[SPECIAL-NAMES. [declaracion-de-nombres-especiales] ...]

[INPUT-OUTPUT SECTION.
FILE-CONTROL. {declaracion-de-control-de-archivos} ...
[I-O-CONTROL. [declaracion-de-control-de-entrada-salida.] ] ]


DATA-DIVISION.

[FILE SECTION.
  [declaracion-de-descripcion-de-archivo.
  [declaracion-de-descripcion-de-registro.] ...] ...]

[WORKING-STORAGE SECTION.
  [declaracion-de-campos-de-nivel-77.] ...
  [declaracion-de-descripcion-de-registro.] ...]

[LINKAGE SECTION.
  [declaracion-de-campos-de-nivel-77.] ...
  [declaracion-de-descripcion-de-registro.] ...]
  
[REPORT SECTION.
  [declaracion-de-descripcion-de-informes.
  [declaracion-de-descripcion-de-grupo-de-informe.] ...] ...]



PROCEDURE DIVISION.

[DECLARATIVES.
[nombre-de-seccion SECTION
  sentencia-declarativa
  [nombre-de-parrafo [sentencia] ...] ...] ...
END DECLARATIVES.]

[nombre-de-seccion SECTION
[nombre-de-parrafo. [sentencia] ...] ...] ...

[END PROGRAM].

```

## 3.4. Elementos del lenguaje COBOL: caracteres y palabras

La unidad más elemental e indivisible en COBOL es el **carácter**.  Los carácteres pueden ser de varios tipos:

* *numéricos*: dígitos entre 0 y 9
* *alfabéticos*: letras ASCII entre "a" y "z", letras entre "A" y "Z"; las letras acentuadas y las eñes no están incluídas.
* *especiales*: `( ) " + - * / = $ , ; < >`
* *adoptados*: "£" en Gran Bretaña, "Ñ" en España, "ç" en Francia, etc.

La combinación de caracteres da lugar a **palabras**. Las palabras pueden ser:

* **palabras reservadas**: tienen un significado predefinido por el COBOL y no es necesario definirlas. Ej.: `AUTHOR`, `INSTALLATION`, `STOP`, etc.
  * constantes figurativas
  * verbos, procedimientos y cláusulas
* **palabras definidas por el programador**: creadas para el programa, las más usuales son los nombres de datos. Ej.: `DNI`, `DOMICILIO`, `SUELDO`, etc.
  * nombres de datos
  * nombres de archivos
  * nombres de párrafo o procedimiento
  * nombres de dispositivo
  * nombres calificados

## 3.5. Palabras definidas por el programador

Veremos el significado y sus reglas de formación.

### 3.5.1. Nombres de datos

Referencias una posición en la memoria del computador, donde está almacenado el valor asociado al nombre de dato.

Deben definirse los nombres de dato en la `DATA DIVISION`.

Para cada nombre se debe especificar el tipo y el tamaño.

### 3.5.2. Nombres de párrafo y procedimiento

**Procedimiento**: *conjunto de instrucciones que se agrupan porque llevan a cabo un proceso determinado; se pueden ejecutar varias veces y llamarse desde distintos puntos del programa.*

**Párrafo**: *conjunto de sentencias COBOL*

### 3.5.3. Nombres de archivo

Son nombres simbólicos que se usan dentro del programa para hacer referencia a *estructuras*  y a los *soportes físicos*  de los datos.

En [Cap 6](06-estructuras-externas.md) se estudiarán los archivos.

### 3.5.4. Nombres de dispositivos periféricos

Identifican un equipo periférico, tal como *impresora*, *disco*, etc.  Dependen del compilador de COBOL utilizado.  Se recomienda consultar el manual del COBOL utilizado.

### 3.5.5. Nombres calificados

Es conveniente utilizar identificadores únicos para cada dato.

Cuando necesitamos usar los mismos nombres, se debe usar un *calificador* que evite la confusión en el compilador

Los calificadores se forman añadiendo las palabras reservadas `OF` o `IN` a los campos de igual nombre, calificándolos con otro identificados, éste si es diferente.

Ejemplo:

```text
  NOMBRE OF EMPLEADO1
  NOMBRE OF EMPLEADO2
```

La ambigüedad en `NOMBRE` desaparece cuando sabemos que es del campo `EMPLEADO1`.

Hay otros nombres definidos por el programador, que se verán más adelante.

### 3.5.6. Reglas de formación de los nombres definidos por el programador

Para **nombres de dato** y **nombres de archivo** las restricciones son:

* caracteres alfabéticos (sin espacios ni eñe)
* pueden usarse dígitos
* puede usarse el guión (`-`)

Ejemplos: `SUELDO`, `APELLIDO-1`

No son válidos: `AÑO`, `APELLIDO 1`

* 30 caracteres es el máximo de longitud para el nombre
* el guión no puede usarse al principio ni al final del nombre, aunque sí en medio
* al menos debe haber un caracter alfabético en la primera posición del nombre
* no se pueden usar signos de puntuación como `, . ;`
* no se pueden usar palabras reservadas

Para **nombres de párrafo** y **nombres de procedimiento** en la `PROCEDURE DIVISION` van las mismas restricciones, 
con la salvedad que no es obligatoria la aparición de un carácter alfabético en el nombre.


## 3.6. Palabras reservadas

Las palabras reservadas tienen un significado predefinido por el compilador.

Son de varios tipos.

### 3.6.1. Constantes figurativas

Es indiferente el uso de singular o plural en el nombre de la constante figurativa.

| constante figurativa | Descripción |
|----------------------|-------------|
| `ZERO`<br>`ZEROS`<br>`ZEROES` | Es el valor cero. Se puede usar como valor numérico en operaciones aritméticas o como literal alfanumérico. |
| `SPACE`<br>`SPACES` | Uno o más espacios en blanco. |
| `QUOTE`<br>`QUOTES` | Una o más comillas.  Como los literales alfanuméricos se ponen entre comillas, se necesita esta constante si se quiere incluirla en un literal. |
| `ALL "Literal"`   | genera tantas apariciones del literal especificado como longitud tenga el campo. | 
| `HIGH-VALUE`<br>`HIGH-VALUES` | el *mayor valor* posible para un dato numerico o alfanumérico, o sea, todo otro dato es menor a él. |
| `LOW-VALUE`<br>`LOW-VALUES` | el *menor valor* posible para un dato numerico o alfanumérico, o sea, todo otro dato es mayor a él. |


### 3.6.2. Verbos, instrucciones y cláusulas

TODO pp. 43
