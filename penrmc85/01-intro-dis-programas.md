# Capítulo 1 - Introducción al diseño de programas


## 1.1. Introducción

Los programas como conjunto de pasos son usuales en las actividades humanas.

## 1.2. Definición de programa

**Programa**: *conjunto de instrucciones escritas en secuencia que, aplicadas o no a un conjunto de datos y ejecutadas por un computador, permiten resolver un problema.*

En informática de gestión, el tratamiento de la información se descompone en tres etapas:


```text
               Entrada de datos -> Proceso -> Salida de datos
```

## 1.3. Componentes de un programa

La información que maneja el computador está compuesta por objetos e instrucciones.
Los objetos son datos de entrada o salida.
Las instrucciones indican las acciones que debe realizar el computador.

### 1.3.1. Objetos de un programa. Datos

Existen dos tipos:

* *constantes* o *literales*: su valor permanece invariable durante la ejecución del programa.
* *variables*: pueden tomar y cambiar su valor durante la ejecución del programa.

Los literales pueden ser:

* *literales numéricos*: ej. `3.1416`   `-4` 
* *literales alfanuméricos*: ej. `"BUENOS DIAS"` cuyos valores van encerrados entre comillas dobles.

A las variables en un programa COBOL se les denomina **campos**. Un campo puede ser:

* *campo simple*: ej. `SUELDO`
* *campo compuesto*: ej. `DIRECCION (CALLE, NUMERO, PISO)` Los campos que forman parte de un campo compuesto se dnominan *subcampos*.

Cada campo, sea simple o compuesto, se define mediante:

* *nombre*: lo identifica
* *tipo*: define el conjunto de valores que puede tomar
* *longitud*: expresada en caracteres u octetos.

El *tipo del campo* puede ser:

* *numérico*: sólo puede contener números
* *alfabético*: sólo puede contener letras
* *alfanumérico*: puede contener números, letras y algunos símbolos (`$`. `%`, etc.)


### 1.3.2. Expresiones. Tipos y clasificación

**Expresión**: *combinación de valores constantes, variables y operadores del lenguaje.*

Las expresiones se clasifican de acuerdo al tipo del resultado en:

* *expresiones numéricas*
* *expresiones alfanuméricas*
* *expresiones lógicas* o *booleanas*: el resultado es verdadero o falso.


Operadores:

| Tipo          | Símbolo | Nombre | Función |
|---------------|---------|--------|---------|
| Aritméticos   | + -     | suma, diferencia producto, división, potencia | |
| Alfanuméricos | +       | Concatenación | |
| Relacionales  | = <> < <= > >= | igual distinto menor ... | |
| Lógicos       | AND OR NOT | conjunción, disyunción inclusiva, negación | |
| Paréntesis    | ( )    |  | |



Las tablas de verdad de AND, OR y NOT se describen a continuación:

| X | Y | X AND Y | X OR Y  | NOT X |
|---|---|---------|---------|-------|
| F | F |    F    |   F     |   V   |
| F | V |    F    |   V     |   V   |
| V | F |    F    |   V     |   F   |
| V | V |    V    |   V     |   F   |


La precedencia de los operadores, de mayor a menor, es:

* Paréntesis.
* Potencias.
* Productos y divisiones.
* Sumas y restas.
* Concatenación.
* Operadores relacionales, todos con igual prioridad.
* NOT.
* AND.
* OR.

Cuando hay más de un operador en una expresión, la evaluación comienza por el de mayor prioridad; si son de la msma prioridad, el orden es de izquierda a derecha.

Sea:

* `CAMPO1`: 4
* `CAMPO2`: 5
* `CAMPO3`: 8

Las  expresiones siguientes evalúan:

| Expresión                                                    | Valor     |
|--------------------------------------------------------------|-----------|
| `CAMPO1 * CAMPO2 - CAMPO3`                                   | 12        |
| `(CAMPO1 + CAMPO2) * CAMPO3`                                 | 72        |
| `CAMPO1 ^ (1 / 2)`                                           | 2         |
| `CAMPO3 - (30 / CAMPO2) > CAMPO1`                            | falso     |
| `((CAMPO3 - CAMPO1) ^ 4 > CAMPO2) AND (CAMPO3 = CAMPO1 * 2)` | verdadero |
| `(CAMPO1 < CAMPO3 - CAMPO2) OR (CAMPO1 - CAMPO3 < 0)`        | verdadero |
| `"BUENOS" + "DIAS" = "BUENOS DIAS"`                          | falso     |



### 1.3.3. Instrucciones de un programa. Tipos

**Instrucción**: *conjunto de una o más palabras reservadas del lenguaje aplicadas a una o más expresiones.*a

La instrucción determina la función a ejecutar y sobre cuáles datos se operará.

Las instrucciones se clasifican de acuerdo a la función que desempeñan dentro del programa.

a. *Instrucciones de declaración*: declaran y definen los objetos del programa.
b. *Instrucciones primitivas*: las que pueden ser ejecutadas por el sistema de manera inmediata.
   * *de asignación*: almacenan en un campo o variable un resultado de una operación.
   * *de entrada*: lee un dato desde un periférico de entrada y lo almacena en un objeto del programa.
   * *de salida*: escribe en un periférico de salida el valor de un objeto del programa.
c. *Instrucciones compuestas*: están formadas por un conjunto de instrucciones primitivas.
d. *Instrucciones de control*: controlan la ejecución del programa:
   * *alternativa*: ejecutan unas u otras instrucciones de acuerdo a una condición.
   * *salto incondicional*: ejecuta una instrucción que no es la consecutiva en el programa.
   * *salto condicional*: si se cumple cierta condición, ejecuta una instrucción que no es la consecutiva en el programa.
   * *iteración o repetición (bucle)*: hace que se repita un conjunto de instrucciones una cierta cantidad fija o variable de veces.


### 1.3.4. Variables auxiliares de un programa

**Variables auxiliares**: son *campos independientes* cuyos valores sirven para controlar procesos o acumular datos.

Ejemplos de variables auxiliares:

* **Interruptor o switch**: se asocia con dos valores (verdadero o falso, 1 ó 0, sí o no)a; sirve para controlde procesos, y para marcar que ha ocurrido cierto suceso.
* **Contador**: se incrementa su valor en una unidad fija, que puede ser negativa.  Cuentan las veces que sucede cierta situación determinada.
* **Acumulador**: suma los valores de otro campo.  Puede usars también un acumulador de productos.

## 1.4. Desarrollo de un programa

TODO

### 1.4.1. Desarrollo de una aplicación

TODO

### 1.4.2. Desarrollo de un programa

TODO

## 1.5. Fase de construcción de un programa

TODO

### 1.5.1.  Definición o planteamiento del problema

TODO

### 1.5.2. Análisis del problema

Se realiza en tres etapas:

1. **Diagrama de proceso.** Se representa el flujo de información, los archivos de entrada y salida, y los periféricos involucrados. FIXME Capítulo 2
2. **Diseño de registros y mensajes.** Se diseñan los registros de entrada, de salida, de entrada/salida y los intermedios si los hubiera.  Se describen los mensajes que se van a mostrar por pantalla y los informes impresos.
3. **Condiciones para la solución.** Se estudian lo temas de performance, comodidad de usuario al usar la aplicación, etc.  Surgen varias alternativas posibles de solución.  Se usan técnicais como programación estructurasa, diseño *top-down*, descomosición en módulos, diagramas HIPO, tablas de decisión.

### 1.5.3. Programación de la solución del problema

Para el **diseño del algoritmo** usaremos *pseudocódigo*.  Luego procederemos a la **codificación del programa** que será traducir el pseudocódigo al lenguaje COBOL.

## 1.6. Fase de edición y puesta a punto

**Edición**: *grabar el programa ya codificado en un archivo.* El resultado es el *programa fuente*.

El programa fuente puede interpretarse o compilarse.  En el caso de COBOL es un lenguaje compilado.a

Es necesario **compilar** el programa fuente, ejecutarlo y verificar que funciona como se esperaba con algún conjunto de *datos de prueba* o *datos de ensayo*.

## 1.7. Fase de documentación del programa

TODO

## 1.8. Fase de explotación del programa

TODO


