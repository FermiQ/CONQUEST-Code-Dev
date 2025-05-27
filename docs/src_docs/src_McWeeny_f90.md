## Overview
To handle the McWeeny/Manolopoulos initialisation of density matrix

## Key Components

### Modules
- `McWeeny`: To handle the McWeeny/Manolopoulos initialisation of density matrix

### Subroutines
- `McWMin(n_L_iterations, deltaE)`: Performs iterative minimisation using McWeeny algorithm
- `InitMcW()`: Initialises L to be a function of H with eigenvalues between 0 and 1
- `McW_matrix_multiply_nonds(matH, matS, matL, matInvS, matRhoNew, energy, c)`: Performs one iteration of McWeeny iterative scheme (non-direct sum version)
- `McW_matrix_multiply_ds(matH, matS, matL, matInvS, matRhoNew, energy_total, c_total)`: Performs one iteration of McWeeny iterative scheme treating the overall matrices L, H as direct sums of their spin components.
- `McWXHX(matA, matB, matBAB, mat_temp)`: Pre- and post- multiplies H by S^-1, and returns the result to both L range (bab) and S range (tm)
- `McWRho0(matA, matB, matC, m, l, n_e, n_o)`: [Add description - likely calculates initial rho]
- `GetHLimits(MinH, MaxH)`: Finds the Gershgorin limits on the eigenvalues of the Hamiltonian

### Functions
None found in this file.

## Important Variables/Constants
[List of important variable/constant names, their types (if determinable), and descriptions of their purpose, especially for parameters or widely used variables. E.g.:
- `variable_name_1` (`type`): Description of its purpose. (Parameter/Constant)
- `variable_name_2` (`type`): Description of its purpose.
]

## Usage Examples
To be added by developers.

## Dependencies and Interactions

### Used Modules
- `global_module, only: io_lun, area_DM, iprint_DM, ni_in_cell, IPRINT_TIME_THRES1, IPRINT_TIME_THRES2, nspin, nspin_SF, flag_fix_spin_population, ne_in_cell, ne_spin_in_cell, spin_factor, flag_SpinDependentSF, min_layer`
- `timer_module, only: start_timer, stop_timer, cq_timer, WITH_LEVEL`
- `timer_stdclocks_module, only: tmr_std_matrices`
- `GenComms, ONLY: inode, ionode, gsum, my_barrier, gmin, gmax, cq_abort, cq_warn`
- `datatypes`
- `numbers`
- `matrix_data, only: Lrange, Srange, TSrange, LSLrange, Hrange`
- `mult_module, only: allocate_temp_matrix, matrix_sum, free_temp_matrix, matL, matH, matS, matT, symmetrise_L, matLS, matSL, mult, matrix_trace, T_S_TS, matrix_product_trace, matrix_product`
- `species_module, only: species, nsf_species`
- `io_module, only: return_prefix`

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
