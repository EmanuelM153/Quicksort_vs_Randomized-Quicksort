# Implementación en Haskell
La implementación en Haskell utiliza listas de diferencia para un mejor rendimiento, y la función `getCPUTime` del módulo `System.CPUTime`, para calcular el tiempo de ordenamiento. Además, es importante resaltar que la versión de Quicksort Randomized utiliza un generador de números aleatorios, que en cada llamada recursiva se divide en dos y es pasado a las dos nuevas invocaciones de las funciones.

## Ejecución
Para ejecutar los programas, una vez compilados, basta con escribir el nombre del ejecutable y pasar como primer parámetro la cantidad de números que se van a ordenar.
```
# Para cien datos
./quicksort 100
./quicksortRandom 100
```

## Pruebas
Las pruebas estan escritas en un bash script y guardan los datos bajo una carpeta llamada `datos`, en archivos separados para cada tamaño del arreglo y la variante del algoritmo utilizada. Para ejecutarlas se puede escribir:
```
bash ./pruebas.sh
```
