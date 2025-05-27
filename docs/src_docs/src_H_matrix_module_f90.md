## Overview
Collects together all routines needed for making the H matrix

## Key Components

### Modules
- `H_matrix_module`: Collects together all routines needed for making the H matrix

### Subroutines
- `get_H_matrix(rebuild_KE_NL, fixed_potential, electrons, rho, size, level, build_AtomF_matrix, transform_AtomF_to_SF)`: gets the entire Hamiltonian matrix
- `get_h_on_atomfns(output_level, fixed_potential, electrons, rho, size)`: Generates H|phi> on the grid
- `get_HNL_matrix()`: Generates the non-local pseudopotential part of the Hamiltonian matrix
- `matrix_scale_diag_tm(matAP, range)`: [Add description]
- `get_HNA_matrix(matNA)`: Generates the neutral-atom matrix using PAO integrals for 1- and 2-centre terms, and projectors for the 3-centre terms
- `matrix_scale_diag_NA(mataNA, range)`: [Add description]
- `get_T_matrix()`: Calculates the kinetic energy matrix elements
- `get_onsite_T(blip_co, mat_KE, np, nn, ip, this_nsf, spec)`: This routine evalutes the kinetic energy matrix elements for the block diagonal.
- `get_output_energies(rho, size)`: Calculate the Hartree and XC energies for a given density
- `matrix_scale_diag(matAP, species, n_projectors, l_core, recip_scale, range)`: Scales the projector-localised orbital scalar product matrix

### Functions
None found in this file.

## Important Variables/Constants
- `locps_output (logical)`: [Add description]
- `flag_write_locps (logical)`: [Add description]
- `max_locps_type (integer, parameter)`: [Add description]
- `flag_dump_locps(max_locps_type) (logical)`: [Add description]

## Usage Examples
To be added by developers.

## Dependencies and Interactions

### Used Modules
- `global_module, only: io_lun, iprint_ops, iprint_exx, flag_basis_set, PAOs, atomf, sf, IPRINT_TIME_THRES1, iprint_SC, flag_perform_cDFT, area_ops, nspin, nspin_SF, spin_factor, blips, flag_analytic_blip_int, flag_neutral_atom, min_layer, flag_pcc_global, exx_alpha, exx_niter, exx_siter, id_glob, species_glob`
- `timer_module, only: start_timer, stop_timer, stop_print_timer, start_backtrace, stop_backtrace, cq_timer, WITH_LEVEL`
- `timer_stdclocks_module, only: tmr_std_hmatrix, tmr_std_allocation, tmr_std_matrices`
- `datatypes`
- `numbers`
- `matrix_data, only: Hrange, Srange, mat, APrange, halo, aNArange, aHa_range`
- `mult_module, only: matNL, matKE, matH, matrix_scale, matrix_sum, matrix_product, matS, matX, matNAatomf, matNA, matNLatomf, matKEatomf, matHatomf, matXatomf, AtomF_to_SF_transform, allocate_temp_matrix, free_temp_matrix, S_trans, matrix_product_trace, mult, AP_PA_aHa, AP_trans, matPA, matAP, scale_matrix_value, store_matrix_value, aNA_NAa_aHa, aNA_trans, matNAa, mataNA, matrix_pos, return_matrix_len, matK, return_matrix_value`
- `pseudopotential_common, only: non_local, pseudopotential, flag_neutral_atom_projector, pseudo_type, OLDPS, SIESTA, STATE, ABINIT`
- `set_bucket_module, only: rem_bucket, atomf_H_atomf_rem, atomf_nlpf_rem`
- `calc_matrix_elements_module, only: get_matrix_elements_new, norb`
- `GenComms, only: gsum, end_comms, my_barrier, inode, ionode, cq_abort, myid`
- `functions_on_grid, only: atomfns, H_on_atomfns, gridfunctions, pseudofns`
- `io_module, only: dump_matrix, dump_blips, write_matrix, dump_locps, return_prefix`
- `dimens, only: n_my_grid_points, grid_point_volume, n_grid_z`
- `memory_module, only: reg_alloc_mem, reg_dealloc_mem, type_dbl`
- `group_module, only: parts`
- `primary_module, only: bundle`
- `cover_module, only: BCS_parts`
- `cdft_data, only: matHzero, cDFT_NumberAtomGroups, cDFT_Vc, matWc`
- `exx_kernel_default, only: get_X_matrix`
- `exx_module, only: get_X_params`
- `exx_types, only: exx_hgrid, exx_psolver, exx_radius`
- `exx_io, only: exx_global_write`
- `energy, only: local_ps_energy, hartree_energy_total_rho, xc_energy, x_energy, delta_E_hartree, hartree_energy_drho, hartree_energy_drho_atom_rho, delta_E_xc`
- `density_module, only: flag_DumpChargeDensity, density_pcc, density_atom`
- `potential_module, only: potential`
- `XC, only: get_xc_potential`
- `GenBlas, only: copy, axpy, dot, rsum, scal, gemm`
- `hartree_module, only: hartree, hartree_stress`
- `fft_module, only: fft3, hartree_factor, z_columns_node, i0`
- `pseudopotential_data, only: n_projectors, l_core, recip_scale`
- `species_module, only: species, nsf_species, nlpf_species`
- `build_PAO_matrices, only: assemble_2`
- `blip_grid_transform_module, only: blip_to_grad_new`
- `blip, only: blip_info`
- `support_spec_format, only: support_function, blips_on_atom`
- `nlpf2blip, only: get_SP, nlpf_on_atom`
- `pao_format, ONLY: pao`
- `pseudo_tm_info, only: pseudo`
- `maxima_module`

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
