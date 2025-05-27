## Overview
Calculates the coefficients used to relate an energy 
tolerance (i.e. dE) to a residual by a process of least
squares fitting, subject to the fit being below all points.

Conquest makes extensive use of residual minimisation 
techniques, and we need a way to relate a tolerance on the
energy (which is easily specified by the user) to the
magnitude of the residual -- hence this routine.

## Key Components

### Modules
- `PosTan`: Calculates the coefficients used to relate an energy tolerance (i.e. dE) to a residual by a process of least squares fitting, subject to the fit being below all points.

### Subroutines
- `fit_coeff(C, beta, E, R, n)`: Fits the coefficients C and beta (such that the tolerance applied to the residual is C*dE^beta where dE is the energy tolerance) to the arrays E and R
- `pos_tan(mxnpt,npt,xpt,ypt,cconst,bslope,myid)`: This subroutine performs the fitting described above.

### Functions
None found in this file.

## Important Variables/Constants
- `max_iters (integer, parameter)`: Maximum number of iterations stored for fitting (Value: 50).
- `eprec (real(double), parameter)`: Precision for energy difference comparison (Value: 1.0e-12).
- `cscale (real(double), parameter)`: Scaling factor for coefficient C (Value: 0.01).
- `PulayE(max_iters) (real(double))`: Array to store energies for Pulay iterations.
- `PulayR(max_iters) (real(double))`: Array to store residuals for Pulay iterations.
- `SCE(max_iters) (real(double))`: Array to store energies for Self-Consistent iterations.
- `SCR(max_iters) (real(double))`: Array to store residuals for Self-Consistent iterations.
- `SupFnE(max_iters) (real(double))`: Array to store energies for Support Function iterations.
- `SupFnR(max_iters) (real(double))`: Array to store residuals for Support Function iterations.
- `PulayC (real(double))`: Fitted coefficient C for Pulay.
- `PulayBeta (real(double))`: Fitted coefficient beta for Pulay.
- `SCC (real(double))`: Fitted coefficient C for Self-Consistent iterations.
- `SCBeta (real(double))`: Fitted coefficient beta for Self-Consistent iterations.
- `SupFnC (real(double))`: Fitted coefficient C for Support Function iterations.
- `SupFnBeta (real(double))`: Fitted coefficient beta for Support Function iterations.

## Usage Examples
To be added by developers.

## Dependencies and Interactions

### Used Modules
- `datatypes`
- `global_module, ONLY: io_lun, area_general, iprint_gen`
- `numbers`
- `GenComms, ONLY: inode, ionode, cq_abort`
- `memory_module, ONLY: reg_alloc_mem, reg_dealloc_mem, type_dbl`
- `functions, ONLY: heapsort_real_index`

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
