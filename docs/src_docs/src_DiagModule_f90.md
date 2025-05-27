## Overview
contains all routines needed to diagonalise the Hamiltonian
Forms the Hamiltonian (by summing over images of local atoms),
distributes it appropriately (i.e. according to Scalapack format)
calls Scalapack and redistributes eigenvectors to build K matrix
This whole module is discussed in exhaustive detail in the
Conquest notes "Implementation of Diagonalisation in Conquest"
(Diagonalisation.tex)

## Key Components

### Modules
- `DiagModule`: contains all routines needed to diagonalise the Hamiltonian

### Subroutines
- `FindEvals(electrons)`: finds the eigenvalues
- `initDiag()`: Contains various routines and assignments that initialise the diagonalisation
- `endDiag()`: Dealloate arrays used for direct diagonalisation calculations
- `PrepareRecv(Distrib)`: Prepares to receive data for Scalapack diagonalisation.
- `PrepareSend(range,Distrib)`: Sets up arrays which are vital to the sending of data when going from CQ format (compressed arrays) to SC format.
- `DistributeCQ_to_SC(Distrib,matA,pgk,SCmat,kpassed)`: Distributes data stored in Conquest compressed row format to processors ready for a Scalapack operation
- `DistributeSC_to_ref(Distrib,ng,SCeig,localEig)`: send data back to processors for reference format
- `findFermi(electrons, eig, nbands, nkp, Ef, occ)`: Finds the fermi level given a set of eigenvalues at a number of k points.
- `findFermi_fixspin(electrons, eig, nbands, nkp, Ef, occ)`: Finds Fermi energy for the case when the populations in the different spin channels are fixed.
- `findFermi_varspin(electrons_total, eig, nbands, nkp, Ef, occ)`: Finds Fermi energy for the case when the populations in the different spin channels are allowed to vary.
- `occupy(occu, ebands, Ef, electrons, nbands, nkp, spin)`: Populates the eigenstates up to a given fermi level at each k-point.
- `buildK(range, matA, occs, kps, weight, localEig, overlapEig, matSij)`: Builds K from eigenvectors
- `write_wavefn_coeffs(eval, evec, spin, tag, firstcall)`: write_wavefn_coeffs
- `get_weight_pDOS(evec,weight_pDOS)`: Normalise projected DOS
- `distrib_and_diag(spin,index_kpoint,mode,flag_store_w,kpassed)`: For k-point i, distributes H and S and diagonalises
- `init_blacs_pg()`: New subroutine which is run once per calculation to initialise BLACS and set up progress groups for Conquest
- `init_scalapack_format()`: New subroutine which is run once per atomic configuration to define the mapping between Conquest and ScaLAPACK
- `end_scalapack_format()`: New subroutine which is run once per atomic configuration to deallocate variables from init_scalapack_format

### Functions
- `fermi(E,kT)` (`real(double)`): evaluate fermi function
- `MP_step(E,order,smear)` (`real(double)`): evaluate Methfessel-Paxton step function
- `MP_entropy(x, order)` (`real(double)`): evaluate the function SN(x) = 0.5*A_N*H_2N(x)*exp(-x^2)

## Important Variables/Constants
**Derived Types:**
- `TYPE location`: Holds the location of a matrix element in data_H.
- `TYPE element`: Holds an array of locations of matrix elements.
- `TYPE DistributeData`: Holds arrays related to distribution of a matrix from Conquest compressed row storage to ScaLAPACK 2D block cyclic storage.
- `TYPE Krecv_data`: Holds various data about matrix elements we're going to build from eigenvectors we'll receive.

**Key Module Variables:**
- `SCHmat, SCSmat, z (complex(double_cplx), allocatable)`: Matrices for ScaLAPACK (Hamiltonian, Overlap, Eigenvectors).
- `Efermi (real(double), dimension(2))`: Fermi Energy.
- `nkp (integer)`: Number of k-points.
- `kT (real(double))`: Smearing temperature for occupations.
- `evals (real(double), allocatable)`: Eigenvalues.
- `occ (real(double), allocatable)`: Occupations.
- `context (integer)`: BLACS context.
- `desca, descb, descz (integer, dimension(50))`: ScaLAPACK descriptors.

## Usage Examples
To be added by developers.

## Dependencies and Interactions

### Used Modules
- `datatypes`
- `global_module, only: io_lun, area_DM, iprint_DM, min_layer, flag_diagonalisation, nspin, spin_factor, flag_fix_spin_population, ne_in_cell, ne_spin_in_cell, flag_DeltaSCF, flag_excite, flag_local_excitation, dscf_HOMO_thresh, dscf_LUMO_thresh, dscf_source_level, dscf_target_level, dscf_target_spin, dscf_source_spin, flag_cdft_atom, dscf_HOMO_limit, dscf_LUMO_limit, flag_out_wf, wf_self_con, max_wf, paof, sf, atomf, out_wf, flag_write_projected_DOS, flag_SpinDependentSF, flag_do_pol_calc, polS, io_ase, write_ase, ase_file, i_pol_dir_end, numprocs, x_atom_cell, y_atom_cell, z_atom_cell, E_wf_max, E_wf_min, flag_wf_range_Ef, id_glob, species_glob, flag_normalise_pDOS, flag_pDOS_angmom, flag_pDOS_lm, iprint`
- `GenComms, only: cq_abort, inode, ionode, myid, cq_warn, my_barrier, mtime, gsum`
- `timer_module, only: start_timer, stop_timer`
- `timer_stdclocks_module, only: tmr_std_matrices`
- `units`
- `ScalapackFormat, only: matrix_size, matrix_size_padH, proc_rows, proc_cols, block_size_r, block_size_c, pg_kpoints, proc_groups, nkpoints_max, pgid, N_procs_in_pg, N_kpoints_in_pg, deallocate_arrays, pg_initialise, ref_to_SC_blocks, make_maps, find_SC_row_atoms, find_ref_row_atoms, find_SC_col_atoms, CC_to_SC, mapy, procid, proc_start, blocks_r, blocks_c`
- `mult_module, only: matH, matS, matK, matM12, SF_to_AtomF_transform, matrix_scale, matrix_product_trace, allocate_temp_matrix, free_temp_matrix, store_matrix_value_pos, matrix_pos, return_matrix_value_pos`
- `matrix_data, only: Hrange, Srange, aHa_range, mat, halo, SFcoeff_range`
- `primary_module, only: bundle`
- `species_module, only: species, nsf_species, species_label, n_species, natomf_species`
- `memory_module, only: type_dbl, type_int, type_cplx, reg_alloc_mem, reg_dealloc_mem`
- `energy, only: entropy`
- `cdft_data, only: cDFT_NumberAtomGroups`
- `maxima_module, ONLY: maxngrid, maxnsf, maxpartscell, maxatomspart`
- `functions_on_grid, only: atomfns, allocate_temp_fn_on_grid, free_temp_fn_on_grid`
- `density_module, ONLY: get_band_density`
- `io_module, ONLY: write_eigenvalues, write_eigenvalues_format_ase, flag_MatrixFile_BinaryFormat, io_assign, io_close`
- `pao_format, ONLY: pao`
- `mpi`
- `numbers, only: zero, minus_i, one, half, two, four, pi`
- `group_module, only: parts`
- `cover_module, only: BCS_parts`
- `GenBlas, only: dot, copy`
- `functions, ONLY: erfc_cq`
- `input_module, only: io_assign, io_close`

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
