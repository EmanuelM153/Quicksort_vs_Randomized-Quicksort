mkdir datos

for pruebaNum in {1000,10000,100000,1000000}
do
    quicksortFile="datos/quicksort_${pruebaNum}"
    quicksortRandomFile="datos/quicksortRandom_${pruebaNum}"
    touch $quicksortFile $quicksortRandomFile
    for _ in {1..100}
    do
        ./quicksort $pruebaNum >>"${quicksortFile}" &
        ./quicksortRandom $pruebaNum >>"${quicksortRandomFile}" &
        wait
    done
done

pruebaFinalNum=10000000
quicksortFile="datos/quicksort_${pruebaFinalNum}"
quicksortRandomFile="datos/quicksortRandom_${pruebaFinalNum}"
for _ in {1..10}
do
    ./quicksort $pruebaFinalNum >>"${quicksortFile}" &
    ./quicksortRandom $pruebaFinalNum >>"${quicksortRandomFile}" &
    wait
done
