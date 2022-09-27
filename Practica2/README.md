<h2 align='center'>
  Práctica 2: ¿Haaaskell? Sí. 
</h2>

<br>

### Alumno

| Alumnos                     | No. de Cuenta |
| --------------------------- | ------------- |
| Paredes Zamudio Luis Daniel | 318159926     |
| Robledo Ramírez Isaac       | 320140655     |

<br>

### Funciones Auxiliares
En la función _pertenece_ usamos en la firma _Eq a =>_ que hace todos los tipos de "a" sean válidos para buscar una equivalencia (==).

<br>

### Nicómano

En la función nicomano lo que pensamos fue primero, validar el dato. Después en salicuotafac clasificamos los factores sumandolos, para que llegando a un entero, vuelva a nicomano en la misma linea, pero ahora con la función equiv. Que clasifica si la suma de factores propios es mayor, menor o igual que el número que se dió. Y va a la función equival que dependiendo el resultado en equival regresa un String. Y con ello nicomano regresa un String.

<br>

### Luhn

En la función luhn pide una lista de elementos de dígitos únicos. Donde una lista de por lo menos de 2 elementos, pasará a primero, ser validada por la función luhndigval que dirá si los enteros en cada elemento son de un solo dígito. En caso que si, pasa a hacer todo el proceso pedido, multSecnLuhn se encarga de multiplicar los dígitos impares, sumaListLuhn de sumar lo elementos de la lista; y modTenLuhn y si la suma es divisible entre 10. Entonces si ambas son verdaderas entonces regresa True.

<br>

### Collatz

La función listaCollatz hace una lista y realiza lo que debe según sea par o impar, hasta que sea 1. Y pasosCollatz da la longitud (con longit) y resta 1 por el paso 0, entonces devuelve la cantidad de elementos - 1 (paso 1 - paso n).

<br>

### Expresiones EA

Esta sección fue la que más se nos complico, ya que aunque entendimos que debia hacer la función, el abstraerlo a nivel de que Haskell lo entendiera. Pudimos con todas menos con las últimas dos (_mayorQue_ y _menorQue_). Sin embargo, dejamos nuestras hipótesis en el archivo de como creeemos que funcionarían (curiosamente, Haskell si entendió, pero nosotros no) 

<br>

### Extras
Al inicio del archivo se agregaron 4 listas se ejemplo para la facilidad de la correcta escritura.

También hicimos muchas secciones con error para evitar entradas inválidas, o que generarían un error.

<br>

> _I've got a bad feeling about this...._
