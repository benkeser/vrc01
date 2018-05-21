#!/bin/bash

sbatch --array=1-493 --time=72:00:00 ./call_reduced_sl_array_ind.sh "ic50" 1

sbatch --array=1-493 --time=72:00:00 ./call_reduced_sl_array_ind.sh "ic50" 2

sbatch --array=1-493 --time=72:00:00 ./call_reduced_sl_array_ind.sh "ic80" 1

sbatch --array=1-493 --time=72:00:00 ./call_reduced_sl_array_ind.sh "ic80" 2

sbatch --array=1-493 --time=72:00:00 ./call_reduced_sl_array_ind.sh "cens" 1

sbatch --array=1-493 --time=72:00:00 ./call_reduced_sl_array_ind.sh "cens" 2

sbatch --array=1-493 --time=72:00:00 ./call_reduced_sl_array_ind.sh "sens.resis" 1

sbatch --array=1-493 --time=72:00:00 ./call_reduced_sl_array_ind.sh "sens.resis" 2

sbatch --array=1-493 --time=72:00:00 ./call_reduced_sl_array_ind.sh "slope_mod" 1

sbatch --array=1-493 --time=72:00:00 ./call_reduced_sl_array_ind.sh "slope_mod" 2