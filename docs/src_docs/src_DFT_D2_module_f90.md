## Overview
! Module DFT_D2
! Code area 9: general
!!****h* Conquest_DFT_D2 *
!! NAME
!!  DFT_D2
!! PURPOSE
!!  Stores various useful dimensions and a subroutine
!! USES
!! AUTHOR
!!  M. Arita
!! CREATION DATE
!!  11/06/09 M. Arita
!! MODIFICATION HISTORY
!!  2022/06/09 12:27 dave
!!   Moved disp_energy to energy module, partition_distance taken
!!   from ion_electrostatic
!! SOURCE

## Key Components

### Modules
- `DFT_D2`: Stores various useful dimensions and a subroutine

### Subroutines
- `dispersion_D2()`: dispersion_D2: Compute the dispersion interaction and the atomic fores
- `set_para_D2()`: set_para_D2: Reads and stores the necessary parameters in the DFT-D2 scheme

### Functions
None found in this file.

## Important Variables/Constants
- `disp_force(:, :)` (`real(double), allocatable`): Stores dispersion forces.
- `atm_disp_coeff(:)` (`real(double), allocatable`): Atomic dispersion coefficients.
- `vdW_rad(:)` (`real(double), allocatable`): Van der Waals radii.
- `disp_stress(3,3)` (`real(double)`): Dispersion contribution to stress tensor.

## Usage Examples
To be added by developers.

## Dependencies and Interactions

### Used Modules
- `datatypes`
- `energy, only: disp_energy`
- `cover_module, ONLY: D2_CS, make_CS`
- `units`
- `dimens, ONLY: atomicnum, r_dft_d2, r_super_x, r_super_y, r_super_z`
- `GenComms, ONLY: cq_abort, gcopy, inode, ionode, gsum, &`
- `group_module, ONLY: parts`
- `global_module, ONLY: io_lun, numprocs, &`
- `numbers`
- `primary_module, ONLY: bundle`
- `io_module, ONLY: get_file_name`
- `species_module, ONLY: n_species, species_label`
- `input_module, ONLY: io_assign, io_close`
- `XC, ONLY: s_6`
- `ion_electrostatic, ONLY: partition_distance`

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
