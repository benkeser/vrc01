#!/bin/bash

sbatch --array=1-12 --time=72:00:00 ./call_reduced_sl_array.sh "ic50" 1

sbatch --array=1-12 --time=72:00:00 ./call_reduced_sl_array.sh "ic50" 2

sbatch --array=1-12 --time=72:00:00 ./call_reduced_sl_array.sh "ic80" 1

sbatch --array=1-12 --time=72:00:00 ./call_reduced_sl_array.sh "ic80" 2

sbatch --array=1-12 --time=72:00:00 ./call_reduced_sl_array.sh "cens" 1

sbatch --array=1-12 --time=72:00:00 ./call_reduced_sl_array.sh "cens" 2

sbatch --array=1-12 --time=72:00:00 ./call_reduced_sl_array.sh "sens.resis" 1

sbatch --array=1-12 --time=72:00:00 ./call_reduced_sl_array.sh "sens.resis" 2

sbatch --array=1-12 --time=72:00:00 ./call_reduced_sl_array.sh "slope_mod" 1

sbatch --array=1-12 --time=72:00:00 ./call_reduced_sl_array.sh "slope_mod" 2