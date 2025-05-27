## Overview
Updates all the necessary data (variables) when running MD
or structure optimisation

## Key Components

### Modules
- `UpdateMember_module`: Updates all the necessary data (variables) when running MD or structure optimisation

### Subroutines
- `group_update_mparts(velocity, flag_empty_bundle)`: Updates the necessary information on group_set 'parts'.
- `deallocate_PSmember()`: Deallocates the necessary arrays of 'bundle' for updates
- `allocate_PSmember(groups)`: Allocates all the necessary arrays for bundle
- `primary_update_mparts()`: Updates the member information in bundle
- `deallocate_CSmember(set)`: Deallocates all the necessary arrays on covering sets for member-updates
- `allocate_CSmember(set,groups,nx_in_cover,ny_in_cover,nz_in_cover, nmodx,nmody,nmodz,dcellx,dcelly,dcellz,prim)`: Allocates all the necessary arrays on covering sets for member-updates
- `cover_update_mparts(set,groups,nx_in_cover,ny_in_cover,nz_in_cover, nmodx,nmody,nmodz,dcellx,dcelly,dcellz,prim)`: Updates all the necessary arrays and variables in covering sets for member-updates
- `updateMembers_group(velocity,flag_empty_bundle)`: Preliminary: update the group
- `updateMembers_cs()`: Updates members of covering sets (atoms only)

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
- `datatypes`
- `global_module, ONLY: flag_MDdebug,iprint_MDdebug, iprint_MD, ni_in_cell,id_glob,id_glob_inv, id_glob_old,id_glob_inv_old, species_glob, x_atom_cell,y_atom_cell,z_atom_cell,atom_coord, flag_neutral_atom, numprocs, flag_dft_d2`
- `timer_module, only: start_timer, stop_timer`
- `timer_stdclocks_module, only: tmr_std_indexing, tmr_std_allocation`
- `basic_types`
- `GenComms, ONLY: cq_abort, ionode, inode, gmax, my_barrier`
- `species_module, ONLY: species`
- `atom_dispenser, ONLY: allatom2part`
- `group_module, ONLY: parts, blocks, deallocate_group_set`
- `primary_module, ONLY: deallocate_primary_set, bundle, make_prim, domain`
- `construct_module, ONLY: init_primary`
- `maxima_module, ONLY: maxatomsproc`
- `io_module, ONLY: get_file_name`
- `input_module, ONLY: io_assign,io_close`
- `cover_module, ONLY: BCS_parts,DCS_parts,ion_ion_CS,D2_CS`
- `functions, ONLY: heapsort_integer_index`

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
