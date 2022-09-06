<h2 align='center'>
  Práctica 1: Introducción a Haskell
</h2>

<br>

### Alumno

| Alumno                      | No. de Cuenta |
| --------------------------- | ------------- |
| Paredes Zamudio Luis Daniel | 318159926     |

<br>

### Funciones Auxiliares

Para par e impar ocupé la función auxiliar vista en clase llamada "myMod". Es nuestra implementación de la función "Mod" de Haskell. 

Para diveE vimos en clase una función auxiliar recursiva llamada diveEAux (renombrada aqui como helpDive). Funciona de esta forma, considerando que recibe un valor 'x', otro 'y' y un contador de ciclos 'count'
- Si x es mayor que y, entonces devuelve el contador
- En caso contrario la función recibe (x-y),y y (count+1) 

De esta forma garantizamos un caso base y un caso recursivo. 

diveE por si sola manda el contador en 0, cumpliendo asi que solo manda enteros. 

<br>

### "Operador" Específico Ocupado.
Haskell tiene un "operador" (no sabría como describirlo bien) llamado "hoogle", que son !!. Haskell lo usa en arreglos a partir de un n dado en la función. Este lo use para la función "Enésimo".

<br>

> _I suggest you try it again...This time, let go you coincious self and act on instinct._
