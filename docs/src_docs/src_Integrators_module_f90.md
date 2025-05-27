## Overview
Evolves particle positions and velocities in MD loop

## Key Components

### Modules
- `Integrators`: Evolves particle positions and velocities in MD loop

### Subroutines
- `vVerlet_r_dt(dt,v,flag_movable)`: Evolve particle positions by dt via velocity Verlet
- `vVerlet_v_dthalf(dt,v,f,flag_movable,second_call)`: Evolve particle velocities by dt/2 via velocity Verlet
- `fire_qMD(dt,v,f,flag_movable,iter,fire_N,fire_N2,fire_P0,fire_alpha)`: performs structure optimisation via (modified) FIRE quenched MD

### Functions
None found in this file.

## Important Variables/Constants
- `fire_N_max (integer)`: Parameters adjusted after this many steps if there is slow convergence (FIRE).
- `fire_N_min (integer)`: Minimum number of steps before adjusting FIRE parameters.
- `fire_N_below_thresh (integer)`: [Add description for fire_N_below_thresh]
- `fire_alpha0 (real(double))`: Initial alpha mixing parameter for FIRE.
- `fire_f_inc (real(double))`: Factor to increase dt in FIRE.
- `fire_f_dec (real(double))`: Factor to decrease dt in FIRE.
- `fire_f_alpha (real(double))`: Factor to adjust alpha in FIRE.
- `fire_max_step (real(double))`: Maximum MD time step in FIRE.

## Usage Examples
To be added by developers.

## Dependencies and Interactions

### Used Modules
- `datatypes`
- `global_module, ONLY: ni_in_cell,id_glob,x_atom_cell,y_atom_cell,z_atom_cell, iprint_MD, io_lun, min_layer, flag_quench_MD, flag_MDdebug`
- `GenComms, only: myid, inode,ionode`
- `species_module, ONLY: species,mass`
- `move_atoms, ONLY: fac`
- `numbers, ONLY: half,zero, one`
- `io_module, ONLY: write_fire`

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
