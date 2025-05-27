## Overview
Collects together routines used in doing Pulay-type
minimisation.  At present only finds the values for
mixing the different iterations

## Key Components

### Modules
- `Pulay`: Collects together routines used in doing Pulay-type minimisation.

### Subroutines
- `DoPulay_nospin(Aij, al, n, max_n)`: Given the matrix A, inverts and sums to create the coefficients alpha (1D Aij array).
- `DoPulay2D_nospin(iPulay, Aij, al, n, max_n)`: As for DoPulay_nospin, but allows the A matrix to be a proper 2D array.
- `DoPulay_spin(iPulay, Aij, alpha, n, max_n)`: Calculates pulay cofficients for spin polarised calculations.

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
- `global_module, ONLY: io_lun, flag_fix_spin_population, ne_in_cell, nspin`
- `GenComms, ONLY: inode, ionode, cq_warn`
- `datatypes`
- `GenBlas, ONLY: sytrf, sytri`
- `numbers`

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
