## Overview
DUMMY module.
Allows people to run Conquest O(N) only without Scalapack or BLACS.
This is a DUMMY module designed to allow those without BLACS and SCALAPACK to compile and run Conquest for order N only.

## Key Components

### Modules
- `DiagModule`: DUMMY module. Allows people to run Conquest O(N) only without Scalapack or BLACS

### Subroutines
- `FindEvals(electrons)`: FindEvals - returns an error and stops. Stop a user without SCALAPACK and BLACS trying to diagonalise

### Functions
None found in this file.

## Important Variables/Constants
- `nkp (integer)`: [Add description - k-point related]
- `wtk (real(double), allocatable, dimension(:))`: [Add description - k-point weights]
- `kk (real(double), allocatable, dimension(:,:))`: [Add description - k-point coordinates]
- `kT (real(double))`: [Add description - Smearing temperature for occupations]
- `maxefermi (integer)`: [Add description - Max iterations for Fermi energy search]
- `flag_smear_type (integer)`: [Add description - Smearing type flag]
- `iMethfessel_Paxton (integer)`: [Add description - Methfessel-Paxton order]
- `gaussian_height (real(double))`: [Add description - Gaussian height for smearing]
- `finess (real(double))`: [Add description - Finess parameter for smearing]
- `NElec_less (real(double))`: [Add description - Electron count parameter for smearing]
- `max_brkt_iterations (integer)`: [Add description - Max bracket iterations for Fermi search]
- `diagon (logical)`: [Add description - Flag for diagonalization vs O(N)]

## Usage Examples
To be added by developers.

## Dependencies and Interactions

### Used Modules
- `datatypes`
- `GenComms, only: cq_abort`

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
