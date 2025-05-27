## Overview
Collects together the routines needed to get the S matrix

## Key Components

### Modules
- `S_matrix_module`: Collects together the routines needed to get the S matrix

### Subroutines
- `get_S_matrix(inode, ionode, build_AtomF_matrix, transform_AtomF_to_SF)`: Gets a new S matrix by doing blip_to_support, integrating and updating inverse S matrix
- `get_S_matrix_PAO(inode, ionode)`: Gets a new S matrix in PAO-basis by assemble_2
- `get_S_matrix_blips(inode, ionode)`: Gets a new S matrix by doing blip_to_support and integrating
- `get_r_on_atomfns(direction,flag_func,inputgf,gridfunc1,gridfunc2,gridfunc3)`: Scales the support functions on the grid by x/y/z
- `Iter_Hott_InvS(output_level, n_L_iterations, tolerance,n_atoms, inode, ionode, flag_do_SFtransform)`: This subroutine finds the inverse of S by Hotelling's method
- `HotInvS_mm( matI, matS, matT0, matT1, matA, omega,n)`: To perform all the required matrix multiplications to produce an inverse S matrix
- `get_onsite_S(blip_co, matS, np, nn, ip, this_nsf, spec)`: This routine evalutes the overlay matrix elements for the block diagonal.
- `get_S_analytic(blipL_co, blipR_co, blip_grad, matS, matT, dataM12, dataK, ip, j_in_halo, dx, dy, dz, speci, specj,this_nsfL,this_nsfR)`: Calculates various matrix elements and gradients analytically from blips.
- `get_dS_analytic_oneL(blipL_co, blipR_co, forS, forT, dataM12, dataK, ip, j_in_halo, dx, dy, dz, speci, specj,this_nsfL,this_nsfR,direction)`: Calculates force contribution for S and KE matrices one direction at a time

### Functions
None found in this file.

## Important Variables/Constants
- `InvSTolerance (real(double))`: Tolerance for S matrix inversion.
- `InvSMaxSteps (integer)`: Maximum steps for S matrix inversion.
- `InvSDeltaOmegaTolerance (real(double))`: Delta Omega tolerance for S matrix inversion.

## Usage Examples
To be added by developers.

## Dependencies and Interactions

### Used Modules
- `datatypes`
- `global_module, ONLY: io_lun, iprint_ops, flag_basis_set, blips, PAOs, atomf, sf, ni_in_cell, nspin_SF, flag_perform_cdft, IPRINT_TIME_THRES1, min_layer, rcellx,rcelly,rcellz, id_glob, species_glob, atom_coord, flag_TmatrixReuse,restart_T, runtype, flag_diagonalisation`
- `timer_module, only: start_timer, stop_timer, start_backtrace, stop_backtrace, cq_timer, WITH_LEVEL`
- `timer_stdclocks_module, only: tmr_std_smatrix`
- `numbers`
- `matrix_data, ONLY: Srange, Trange, TSrange, mat, halo, blip_trans, aSa_range`
- `mult_module, ONLY: matS, matSatomf, AtomF_to_SF_transform, allocate_temp_matrix, free_temp_matrix, store_matrix_value, matrix_scale, matrix_sum, matT, return_matrix_value, T_trans, mult, T_S_TS, TS_T_T, matrix_product_trace, matK, matM12, return_matrix_block_pos, matKE, matrix_pos`
- `io_module, ONLY: dump_matrix, return_prefix`
- `cdft_module, ONLY: make_weights`
- `build_PAO_matrices,          only: assemble_2`
- `PAO_grid_transform_module,   only: PAO_or_gradPAO_to_grid, check_block`
- `functions_on_grid,           only: atomfns, gridfunctions, fn_on_grid`
- `angular_coeff_routines,      only: evaluate_pao`
- `set_bucket_module,           only: rem_bucket`
- `calc_matrix_elements_module, only: get_matrix_elements_new`
- `blip_grid_transform_module,  only: blip_to_support_new`
- `primary_module ,             only: bundle, domain`
- `support_spec_format,         only: blips_on_atom, support_function, coefficient_array, support_gradient, grad_coeff_array`
- `species_module,              only: nsf_species, species, natomf_species`
- `comms_module,                ONLY: start_blip_transfer, fetch_blips`
- `group_module,                ONLY: parts, blocks`
- `blip,                        ONLY: blip_info`
- `cover_module,                ONLY: BCS_parts, DCS_parts`
- `GenComms,                    ONLY: myid, cq_abort, my_barrier, mtime, inode, ionode`
- `mpi`
- `dimens,                      only: n_my_grid_points, r_h, r_super_x, r_super_y, r_super_z`
- `input_module, ONLY: leqi`
- `store_matrix, ONLY: dump_matrix2, grab_matrix2, InfoMatrixFile, matrix_store_global, grab_InfoMatGlobal, set_atom_coord_diff`
- `UpdateInfo, ONLY: Matrix_CommRebuild`
- `GenBlas, ONLY: axpy, copy, scal, gemm`
- `nlpf2blip, ONLY: do_blip_integrals, do_dblip_integrals, do_d2blip_integrals, do_d3blip_integrals`

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
