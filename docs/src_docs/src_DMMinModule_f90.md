## Overview
Contains all routines related to minimisation of energy with
respect to elements of density matrix

Contains initialisation, early stage (steepest descents) and late
stage (GR-Pulay - D.R.Bowler and M.J.Gillan, Chem. Phys.
Lett. 325, 473 (2000)) techniques as well as a call to a
diagonalisation routine for exact solution

## Key Components

### Modules
- `DMMin`: Contains all routines related to minimisation of energy with respect to elements of density matrix

### Subroutines
- `FindMinDM(n_L_iterations, vary_mu, tolerance, resetL, record, level)`: FindMinDM - control routine for energy minimisation wrt DM
- `earlyDM(ndone, n_L_iterations, delta_e, done, vary_mu, tolerance, inflex, record)`: earlyDM - early stage DM min
- `lateDM(ndone, n_L_iterations, done, deltaE, vary_mu, tolerance, record)`: Performs late-stage (linear) minimisation of energy wrt density matrix elements.
- `dealloc_lateDM()`: [Add description]
- `lineMinL(matM3, mat_D, mat_temp, matSM3, energy_in, energy_out, delta_e, inflex, interpG)`: Performs an analytical line minimisation in the direction given by mat_D.
- `correct_electron_number()`: Selector subroutine for correct_electron_number
- `correct_electron_number_fixspin()`: correct_electron_numbers for each spin component, treating the spin populations fixed
- `correct_electron_number_varspin()`: For spin polarised calculations: Corrects the L matrices so that the electron number is correct treating the overall L matrix as direct sum of the spin channels.

### Functions
- `SolveCubic(a,b,c,guess,inode,ionode)` (`real(double)`): Solves a cubic: x^3+Ax^2+Bx+C

## Important Variables/Constants
- `maxpulayDMM (integer)`: Max Pulay mixing history.
- `LinTol_DMM (real(double))`: Linearity tolerance for DM minimisation.
- `minpulaystepDMM (real(double))`: Min step size for Pulay.
- `maxpulaystepDMM (real(double))`: Max step size for Pulay.
- `dE_DMM (real(double), save)`: Change in energy during DM minimisation.

## Usage Examples
To be added by developers.

## Dependencies and Interactions

### Used Modules
- `datatypes`
- `global_module, only: io_lun, area_DM, iprint_DM, IPRINT_TIME_THRES1, nspin, nspin_SF, flag_fix_spin_population, ne_in_cell, ne_spin_in_cell, flag_dump_L, flag_SkipEarlyDM, flag_XLBOMD, flag_propagateX, flag_dissipation, integratorXL, runtype, flag_exx, flag_diagonalisation, min_layer, flag_DM_converged`
- `timer_module, only: start_timer, stop_timer, cq_timer, start_backtrace, stop_backtrace, stop_print_timer, WITH_LEVEL`
- `timer_stdclocks_module, only: tmr_std_densitymat`
- `GenComms, ONLY: inode, ionode, cq_abort, gsum, cq_warn`
- `mult_module, only: matrix_transpose, matT, matTtran, matL, matS, matrix_sum, matK, matphi, mult, T_L_TL, TL_T_L, matrix_product_trace, LNV_matrix_multiply, allocate_temp_matrix, free_temp_matrix, matrix_product, symmetrise_L, matXL, matXLvel`
- `McWeeny, only: InitMcW, McWMin`
- `PosTan, only: max_iters, cscale, PulayE, PulayR, PulayC, PulayBeta, pos_tan, fit_coeff`
- `DiagModule, only: FindEvals`
- `io_module, only: dump_matrix, return_prefix, dump_charge`
- `energy, only: entropy`
- `matrix_data, only: Lrange, Srange, LSrange, Hrange, TLrange`
- `store_matrix, only: dump_XL, dump_pos_and_matrices, unit_DM_save`
- `exx_kernel_default, only: get_X_matrix`
- `logicals`
- `numbers`
- `primary_module, only: bundle`
- `Pulay`
- `functions_on_grid, only: atomfns, H_on_atomfns`
- `H_matrix_module, only: get_H_matrix`
- `density_module, only: density, get_electronic_density, flag_DumpChargeDensity`
- `maxima_module, only: maxngrid`
- `memory_module, only: reg_alloc_mem, reg_dealloc_mem, type_dbl`
- `dimens, only: n_my_grid_points`

### Called Subroutines/Functions (if determinable)
[List of other subroutines/functions within the project that this file calls. E.g.:
- `project_subroutine_alpha()`
- `project_function_beta()`
]

### External Library Dependencies
[List of any external libraries used, e.g.:
- `MPI`
- `LibXC`
- `FFTW`
]
