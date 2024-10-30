import random
import time
import numpy as np
import matplotlib.pyplot as plt
import tracemalloc

# Implementación de QuickSort
def quicksort(arr):
    if len(arr) <= 1:
        return arr
    pivot = arr[len(arr) // 2]
    left = [x for x in arr if x < pivot]
    middle = [x for x in arr if x == pivot]
    right = [x for x in arr if x > pivot]
    return quicksort(left) + middle + quicksort(right)

# Implementación de Randomized QuickSort
def randomized_quicksort(arr):
    if len(arr) <= 1:
        return arr
    pivot = random.choice(arr)
    left = [x for x in arr if x < pivot]
    middle = [x for x in arr if x == pivot]
    right = [x for x in arr if x > pivot]
    return randomized_quicksort(left) + middle + randomized_quicksort(right)

# Función para medir tiempo y memoria
def medir_tiempo_y_memoria(func, arr, repeticiones):
    tiempos = []
    memorias = []
    for _ in range(repeticiones):
        # Medición de tiempo
        inicio_tiempo = time.time()
        
        # Medición de memoria
        tracemalloc.start()
        func(arr.copy())
        memoria_actual, memoria_maxima = tracemalloc.get_traced_memory()
        tracemalloc.stop()

        fin_tiempo = time.time()
        
        tiempos.append(fin_tiempo - inicio_tiempo)
        memorias.append(memoria_maxima / (1024 ** 2))  # Convertir a MB

    tiempo_promedio = np.mean(tiempos)
    memoria_promedio = np.mean(memorias)
    return tiempo_promedio, memoria_promedio

# Configuración de tamaños de entrada y repeticiones
tamanos = [1000, 10000, 100000, 1000000, 10000000]
resultados_qs_tiempos = []
resultados_qs_memorias = []
resultados_rqs_tiempos = []
resultados_rqs_memorias = []

# Realizar los experimentos
for tamano in tamanos:
    arr = [random.randint(1, 100000) for _ in range(tamano)]
    repeticiones = 10 if tamano == 10000000 else 100

    tiempo_qs, memoria_qs = medir_tiempo_y_memoria(quicksort, arr, repeticiones)
    tiempo_rqs, memoria_rqs = medir_tiempo_y_memoria(randomized_quicksort, arr, repeticiones)

    resultados_qs_tiempos.append(tiempo_qs)
    resultados_qs_memorias.append(memoria_qs)
    resultados_rqs_tiempos.append(tiempo_rqs)
    resultados_rqs_memorias.append(memoria_rqs)

# Mostrar resultados en una tabla
print("Tamaño de Datos | QuickSort (s) | QuickSort (MB) | Randomized QuickSort (s) | Randomized QuickSort (MB)")
for i, tamano in enumerate(tamanos):
    print(f"{tamano:<15} | {resultados_qs_tiempos[i]:<14} | {resultados_qs_memorias[i]:<12} | {resultados_rqs_tiempos[i]:<22} | {resultados_rqs_memorias[i]:<16}")

# Generación de la gráfica (Tiempo)
plt.figure()
plt.plot(tamanos, resultados_qs_tiempos, 'bo-', label='QuickSort (Tiempo)')
plt.plot(tamanos, resultados_rqs_tiempos, 'ro-', label='Randomized QuickSort (Tiempo)')
plt.xscale('log')
plt.yscale('log')
plt.xlabel('Cantidad de datos')
plt.ylabel('Tiempo de ejecución promedio (s)')
plt.title('Comparación de Tiempo - QuickSort VS Randomized QuickSort')
plt.legend()
plt.grid(True, which="both", ls="--")
plt.show()

# Generación de la gráfica (Memoria)
plt.figure()
plt.plot(tamanos, resultados_qs_memorias, 'bo-', label='QuickSort (Memoria)')
plt.plot(tamanos, resultados_rqs_memorias, 'ro-', label='Randomized QuickSort (Memoria)')
plt.xscale('log')
plt.xlabel('Cantidad de datos')
plt.ylabel('Uso de memoria promedio (MB)')
plt.title('Comparación de Memoria - QuickSort VS Randomized QuickSort')
plt.legend()
plt.grid(True, which="both", ls="--")
plt.show()
