# CI3661_Proyecto_I_RARisimo

## Explicación de como funciona el oráculo

El oráculo tiene 4 opciones y la verdadera complejidad era en almacenar el árbol

### Codificar

Para codificar lo primero que debemos hacer es la lectura del archivo

Con el archivo en un string ahora debemos construir su arbol y encriptarlo

¿Cómo se guarda?

Debemos guardar 4 cosas:

Bits despreciados, tamaño del arbol, arbol, contenido codificado

#### Bits despreciados: 
Si el contenido encriptado no da un multilpo de 8 bits se guarda diferente

```
111111
```

se guarda como

```
00111111
```

Entonces en el ultimo byte hay unos 0's sobrante, el primer byte (8 bits) dice cuantos son los bits despreciados (podria ser 3 bits pero tendriamos el mismo problema)

#### Tamaño del arbol
Lo que mide el show tree en bytes, es un int de 32 bits (un poco exagerado pero es mejor asegurar para casos masivos)

#### Arbol
El showtree pasado a binario

#### Codificacion
Los bits resultantes de codificar, recordemos que al guardar tenemos que ignorar los primeros 0's del ultimo byte

Finalmente guardamos el archivo con todo esto

### Decodificar

Tomando en cuenta el punto anterior

Primero agarramos el primer byte y lo guardamos

Luego agarramos los siguientes 4 bytes y transformamos a int

Dividimos lo que queda segun lo que nos indique el int

Y ahora debemos transformar de binario a string el arbol y pasarlo, con el arbol debemos simplemente meter el contenido

con el contenido hacemos la decodficacion y la imprimos

### Analizar

Usamos la funcion de codificar pero no guardamos el archivo y calculamos el rendiento:

$r = 100\times (1- \dfrac{c}{i} )$

Donde:

r: rendimiento

c: largo codificado

i: largo inicial
### Salir

Sales de la funcion
