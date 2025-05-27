## Overview
Transforms support functions represented in a PAO basis onto integration grid

This follows rather closely the blip_grid_transform_module

## Key Components

### Modules
- `PAO_grid_transform_module`: Transforms support functions represented in a PAO basis onto integration grid

### Subroutines
- `PAO_or_gradPAO_to_grid(pao_fns, evaluate, direction)`: Projects the PAO functions onto the grid (rather than support functions). Used for gradients of energy wrt PAO coefficients
- `check_block(xblock,yblock,zblock,xatom,yatom,zatom,rcut, npoint, ip_store, r_store, x_store, y_store, z_store,blocksize)`: finds integration grids in a given block whose distances from the atom (xatom, yatom, zatom) are within a cutoff 'rcut'.
- `get_species(iblock, naba_part_label, ind_part, iatom, icover, my_species)`: Helper routine to get species and icover from block, particle and atom indices.

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
- `global_module, ONLY: io_lun, rcellx,rcelly,rcellz,id_glob,ni_in_cell, iprint_basis, species_glob, atomf`
- `timer_module, only: start_timer, stop_timer`
- `timer_stdclocks_module, only: tmr_std_basis, tmr_std_allocation`
- `datatypes, only: double`
- `primary_module, ONLY: bundle, domain`
- `GenComms, ONLY: my_barrier, cq_abort, mtime, inode, ionode`
- `dimens, ONLY: r_h`
- `numbers`
- `species_module, ONLY: species, npao_species`
- `block_module, ONLY : nx_in_block,ny_in_block,nz_in_block, n_pts_in_block`
- `group_module, ONLY : blocks, parts`
- `cover_module, ONLY: DCS_parts`
- `set_blipgrid_module, ONLY : naba_atoms_of_blocks`
- `functions_on_grid, ONLY: gridfunctions, fn_on_grid`
- `pao_format`

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
