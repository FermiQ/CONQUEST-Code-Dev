## Overview
Contains master subroutines used in self-consistency, and various
useful variables.  Both "early" and "late" stage self-consistency
strategies are described in the notes "How to achieve
self-consistency" by MJG to be found in the Conquest
documentation repository, also in Chem. Phys. Lett.  325, 796
(2000) and in pieces in Cquest (my directory NewSC)

## Key Components

### Modules
- `SelfCon`: Contains master subroutines used in self-consistency, and various useful variables.

### Subroutines
- `new_SC_potl(record, self_tol, reset_L, fixed_potential, vary_mu, n_L_iterations, L_tol, total_energy, level)`: This controls the switch from early-stage to late-stage mixing.
- `lateSC(record, done, ndone, self_tol, reset_L, fixed_potential, vary_mu, n_L_iterations, L_tol, total_energy, rho, size, level)`: This implements late-stage mixing (using the GR-Pulay algorithm).
- `allocate_PulayMixSC_spin()`: [Add description - nested helper]
- `deallocate_PulayMixSC_spin()`: [Add description - nested helper]
- `PulayMixSC_spin(done, ndone, self_tol, reset_L, fixed_potential, vary_mu, n_L_iterations, L_tol, total_energy, rho, size, level)`: Performs Pulay mixing of charge density with Kerker preconditioning and wavedependent metric.
- `get_pulay_optimal_rho(iPulay, rho_opt, pul_max, mix_factor, rho_pul, resid_pul, k_resid_pul, cov_resid_pul, level)`: From a given pulay history calculated optimised and mixed density.
- `update_pulay_history(iPulay, rho_in, reset_L, fixed_potential, vary_mu, n_L_iterations, L_tol, total_energy, rho_pul, resid_pul, k_resid_pul, cov_resid_pul, level)`: From input rho_in, calculated the corresponding residual and electron numbers and store in iPulay slot of the pulay history.
- `get_new_rho(record, reset_L, fixed_potential, vary_mu, n_CG_L_iterations, tolerance, total_energy, rhoin, rhoout, size, level)`: Gets a new charge density when given an input one.
- `get_atomic_charge()`: Computes and prints atomic charges (Mulliken analysis).

### Functions
None found in this file.

## Important Variables/Constants
- `thresh (real(double), parameter)`: Value `2.0_double`. [Add description]
- `InitialLambda (real(double), parameter)`: Value `1.0_double`. [Add description]
- `ReduceLimit (real(double), parameter)`: Value `0.5_double`. [Add description]
- `crit_lin (real(double), parameter)`: Value `0.1_double`. [Add description]
- `maxitersSC (integer)`: Maximum SCF iterations.
- `minitersSC (integer)`: Minimum SCF iterations.
- `maxearlySC (integer)`: Maximum early stage SCF iterations.
- `maxpulaySC (integer)`: Maximum Pulay history.
- `EarlyRecord (integer, allocatable, dimension(:))`: Record of early stage iterations.
- `earlyIters (integer, save)`: Counter for early stage iterations.
- `earlyL (logical)`: Flag for early stage.
- `MixLin (logical, parameter)`: Value `.true.`. [Add description]
- `n_exact (integer)`: [Add description]
- `atomch_output (logical)`: Flag for atomic charge output.
- `flag_Kerker (logical, save)`: Flag for Kerker preconditioning.
- `flag_wdmetric (logical, save)`: Flag for wave-dependent metric.
- `flag_newresidual (logical, save)`: Flag for new residual definition.
- `flag_newresid_abs (logical, save)`: Flag for absolute new residual.
- `A (real(double), dimension(2), save)`: Mixing factor for spin channels.
- `q0 (real(double), save)`: Kerker preconditioning parameter.
- `q1 (real(double), save)`: Wave-dependent metric parameter.
- `flag_linear_mixing (logical, save)`: Flag to use linear mixing.
- `EndLinearMixing (real(double), save)`: Residual threshold to switch from linear mixing.
- `dE_SCF (real(double), save)`: Change in energy during SCF cycle.
- `n_dumpSCF (integer, save)`: Frequency to dump K-matrix for SCF.

## Usage Examples
To be added by developers.

## Dependencies and Interactions

### Used Modules
- `datatypes`
- `global_module, ONLY: io_lun, area_SC, iprint_SC, flag_self_consistent, flag_SCconverged, IPRINT_TIME_THRES2, nspin, spin_factor, min_layer, flag_mix_L_SC_min, ne_in_cell, flag_continue_on_SC_fail, flag_fix_spin_population, ne_spin_in_cell`
- `timer_module, only: start_timer, stop_timer, cq_timer, start_backtrace, stop_backtrace, WITH_LEVEL`
- `timer_stdclocks_module, only: tmr_std_chargescf, tmr_std_allocation`
- `PosTan`
- `numbers`
- `Pulay, only: DoPulay`
- `GenBlas, only: dot, rsum, axpy`
- `dimens, only: n_my_grid_points, grid_point_volume, volume`
- `GenComms, only: gsum, cq_abort, inode, ionode, my_barrier`
- `io_module, only: dump_charge, return_prefix`
- `hartree_module, only: kerker, kerker_and_wdmetric, wdmetric`
- `memory_module, only: reg_alloc_mem, reg_dealloc_mem, type_dbl`
- `maxima_module, only: maxngrid`
- `store_matrix, only: dump_pos_and_matrices, unit_SCF_save`
- `H_matrix_module, only: get_H_matrix, get_output_energies`
- `DMMin, only: FindMinDM, dE_DMM`
- `energy, only: get_energy`
- `functions_on_grid, only: atomfns, H_on_atomfns, allocate_temp_fn_on_grid, free_temp_fn_on_grid`
- `density_module, only: density, get_electronic_density, flag_DumpChargeDensity`
- `cdft_module, only: cdft_min`
- `multisiteSF_module, only: initial_SFcoeff, flag_mix_LFD_SCF`
- `matrix_data, only: Srange`
- `atoms, only: atoms_on_node`
- `input_module, only: io_assign, io_close`

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
