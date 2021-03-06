# Prediction of VRC01 neutralization sensitivity by HIV-1 gp160 sequence features

**Authors:** Craig A. Magaret, David C. Benkeser, Brian D. Williamson, Bhavesh R. Borate, Lindsay N. Carpp, Ivelin S. Georgiev, Ian Setliff, Adam S. Dingens, Noah Simon, Marco Carone, David Montefiori, Galit Alter, Wen-Han Yu, Michal Juraska, Paul T. Edlefsen, Shelly Karuna, Nyaradzo M. Mgodi, Srilatha Edugupanti, Peter B. Gilbert 

-----

## Objective 2 code

The code for objective two can be conceptualized in two work flows. The first
computes variable importance metrics based on metrics proposed in Williamson
2018 (submitted). The second computes variable importance metrics based on
repeated Monte Carlo cross-validation and compiles results. Here, we provide a
short orientation to these work flows.

### Work flow 1

This second work flow consists of the following sequence of `R` and shell scripts:
1. `01-get_fits_from_objective_1.R` (which `source`s `00-ensemble_vim_helpers.R`); then 
2. `02-second_stage_regression_group.R` (general purpose script to run second
stage regression, holding out a group of features), or
3. `02-second_stage_regression_individual.R` (general purpose script to run
second stage regression, holding out a single feature), via either
4. either `shell_scripts_hpc/submit_groups.sh` (running on a SLURM cluster) or
`shell_scripts_local/run_groups.sh` (running on a local machine), or
5. either `shell_scripts_hpc/submit_ind.sh` (running on a SLURM cluster) or
`shell_scripts_local/run_ind.sh` (running on a local machine); finally,
6. `03-ensemble_vimp_analysis.R`

### Work flow 2

The first work flow consists of the following ordered sequence of `R` scripts:
1. `01_prediction_mccv_v4.R` (which `source`s `00_predlib_v3.Rlib`)
2. `02_compile_results_v3.R`
3. `03_compile_varimport_v2.R`
4. `04_varimport_omnibus_v2.R`
5. `05_tables_2_3_v3.R`
6. `06_reformat_tables.R`
7. `07_figure_7_v2.R`
8. `08_figure_6ab_v3.R`

We remind users that the lines of code setting the value of the object
`path.home` need to be modified to point to the directory of the code
repository on their local machine. A similar step is seen in `01_get_fits_from_objective_1.R`. 

This work flow also makes use of the `/code/data/` subdirectory, which contains
several files that are used to make the figures and tables. Here, we provide a brief description of these data files.
- `cd4_bsites.dat`:  a text file containing HXB2 positions of CD4 binding sites.
- `vrc01_bsites.csv`:  a CSV file containing the sites documented to be in the VRC01 binding footprint.
- `dssp_group_a.dat`, `dssp_group_b.dat`:  the sites provided by Ivelin G., computationally estimated to have sufficient exposed surface area for VRC01 binding.  Group "a" have the most surface area, group "b" has slightly less.  We used the union of "a" and "b".  Not included in group "c", which we judged to be too lenient for inclusion.
- `hxb2.map`:  a pipe-delimited file describing the positions of the CATNAP alignment and how they map to HXB2.


-----

## Issues

If you encounter any bugs or have any specific questions about the analysis, please
[file an issue](https://github.com/benkeser/vrc01/issues).

