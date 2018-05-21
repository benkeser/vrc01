#!/bin/bash

#!/bin/bash

outcome = "ic50"
for set in {1,2} 
do
    for grp in {1..493}
    do
        ./call_reduced_sl_ind.sh outcome set grp 
    done
done

outcome = "ic80"
for set in {1,2} 
do
    for grp in {1..493}
    do
        ./call_reduced_sl_ind.sh outcome set grp 
    done
done

outcome = "cens"
for set in {1,2} 
do
    for grp in {1..493}
    do
        ./call_reduced_sl_ind.sh outcome set grp 
    done
done

outcome = "sens.resis"
for set in {1,2} 
do
    for grp in {1..493}
    do
        ./call_reduced_sl_ind.sh outcome set grp 
    done
done

outcome = "slope_mod"
for set in {1,2} 
do
    for grp in {1..493}
    do
        ./call_reduced_sl_ind.sh outcome set grp 
    done
done
