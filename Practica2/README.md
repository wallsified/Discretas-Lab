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

## Funciones Auxiliares
En la función _pertenece_ usamos en la firma _Eq a =>_ que hace todos los tipos de "a" sean válidos para buscar una equivalencia (==).

### Nicómano

_equival_: Es como el ejemplo de clase donde se hace la equivalencia de los planetas, solo que aqui devuelve un String y no un float.

_salicuotafac_: Obtiene los factores propios y los suma

_equiv_: Clasifica el valor de la suma de los factores con respecto al número que el usuario dió.

#### Luhn

_luhndigval_: Verifica que todo elemento en la lista sea entre 0 y 9. Así se pueden hacer núemros de 1 solo dígito.

_multSecnLuhn_: Multiplica el primer elemnto por 2 y el otro no. Si es uno solo, ya sea ej. [1,2,1,2,1,2,1], el último debe ser múltiplicado por 2.

_restNineLuhn_: Resta 9 si es mayor a 9.

_sumaListLuhn_: Suma los elementos en la lista

_modTenLuhn_: Si es módulo de 10, regresa True, en caso contrario False.

### Collatz

_longit_: Regresa el número de elementos en una lista.

_div_: Realiza una división. (Función del Prelude)

<br>

### Extras
Al inicio del archivo se agregaron 4 listas se ejemplo para la facilidad de la correcta escritura.

También hicimos muchas secciones con error para evitar entradas inválidas, o que generarían un error.

<br>

> _I've got a bad feeling about this...._
