## Overview
Reads files, performs communication and reconstructs matrices.
The following subroutines are applicable not only to L-matrix
but also to any sort of matrices.

## Key Components

### Modules
- `UpdateInfo`: Reads files, performs communication and reconstructs matrices.

### Subroutines
- `Matrix_CommRebuild(InfoGlob,InfoMat,range,trans,matA,nfile,symm,n_matrix)`: Manages communication and reordering of matrix elements
- `make_glob2node()`: Makes the table showing which processors atoms belong to.
- `sort_recv_node(x,n,x_order,comm)`: Performs heap-sort to decide the node order for MPI communication.
- `alloc_send_array(nfile,LmatrixSend,isend_array,isend2_array,send_array,InfoMat)`: Organizes local/remote atoms and prepares arrays for sending data.
- `CommMat_send_size(LmatrixSend,isend_array)`: Sends the array size for communication.
- `CommMat_send_neigh(LmatrixSend,isend2_array,InfoMat)`: Sends the data on neighbours.
- `CommMat_send_data(LmatrixSend,send_array,InfoMat)`: Sends interatomic distances and matrix elements.
- `alloc_recv_array(InfoGlob,irecv_array,irecv2_array,recv_array,LmatrixRecv,isize1,isize2,flag_remote_iprim)`: Checks atom locality, sorts sending nodes, and allocates arrays for receiving data.
- `CommMat_irecv_data(LmatrixRecv,irecv2_array,recv_array,nreq2,nreq3)`: Receives neighbour data, interatomic distances and matrix elements.
- `UpdateMatrix_local(InfoMat,range,n_matrix,matA,flag_remote_iprim,nfile)`: Gets and constructs matrix with i_new and j_new for local atoms.
- `UpdateMatrix_remote(range,n_matrix,matA,LmatrixRecv,flag_remote_iprim,irecv2_array,recv_array)`: Gets and constructs matrix with i_new and j_new for remote primary atoms.
- `deallocate_CommMatArrays(sarray1,sarray2,sarray3,rarray1,rarray2,rarray3,LmatrixSend,LmatrixRecv)`: Releases memories of the arrays used in communication.
- `Report_UpdateMatrix(matname)`: Reports the statistics of UpdateMatrix_local and UpdateMatrix_remote.

### Functions
- `WhichNode(inode_old)` (`integer`): Get who the sender is by his node ID.

## Important Variables/Constants
**Derived Types:**
- `TYPE Lmatrix_comm_send`: Holds data for sending matrix information.
  - `nspin (integer)`: Number of spin components.
  - `natom_remote (integer)`: Number of remote atoms.
  - `nrecv_node (integer)`: Number of receiving nodes.
  - `list_recv_node (integer, pointer, dimension(:))`: List of receiving node IDs.
  - `natom_recv_node (integer, pointer, dimension(:))`: Number of atoms to send to each receiving node.
  - `atom_ifile (integer, pointer, dimension(:))`: File index for each atom.
  - `atom_ia_file (integer, pointer, dimension(:))`: Atom index within its file.
- `TYPE Lmatrix_comm_recv`: Holds data for receiving matrix information.
  - `nspin (integer)`: Number of spin components.
  - `natom_remote (integer)`: Number of remote atoms this node will receive data for.
  - `nsend_node (integer)`: Number of sending nodes.
  - `list_send_node (integer, pointer, dimension(:))`: List of sending node IDs.
  - `natom_send_node (integer, pointer, dimension(:))`: Number of atoms expected from each sending node.
  - `ibeg_send_node (integer, pointer, dimension(:))`: Starting index for data from each sending node.
  - `id_prim_recv (integer, pointer, dimension(:))`: Primary atom ID (local bundle index) for received data.
  - `nalpha_prim_recv (integer, pointer, dimension(:))`: Number of alpha functions for the primary atom.
  - `nj_prim_recv (integer, pointer, dimension(:))`: Number of j neighbors for the primary atom.
  - `njbeta_prim_recv (integer, pointer, dimension(:))`: Max number of beta functions for any j neighbor.
  - `ibeg1_recv_array (integer, pointer, dimension(:))`: Starting index in `irecv2_array`.
  - `ibeg2_recv_array (integer, pointer, dimension(:))`: Starting index in `recv_array`.

**Key Module Variables (for reporting):**
- `matrix_name (character(len=4))`: Name of the matrix being updated (e.g., 'Kmat').
- `ifile_local, iatom_local, ij_local, ij_success_local, ij_fail_local (integer)`: Counters for local matrix update statistics.
- `min_Rij_fail_local, max_Rij_fail_local, ave_Rij_fail_local (real(double))`: Statistics for failed local pair updates.
- `ifile_remote, iatom_remote, ij_remote, ij_success_remote, ij_fail_remote (integer)`: Counters for remote matrix update statistics.
- `min_Rij_fail_remote, max_Rij_fail_remote, ave_Rij_fail_remote (real(double))`: Statistics for failed remote pair updates.

## Usage Examples
To be added by developers.

## Dependencies and Interactions

### Used Modules
- `datatypes`
- `global_module, ONLY: flag_MDdebug,iprint_MDdebug, iprint_MD, glob2node,ni_in_cell,numprocs,io_lun, runtype, rcellx, rcelly, rcellz, sf, nlpf, atomf, atom_coord, shift_in_bohr`
- `mpi`
- `GenComms, ONLY: cq_abort,inode,ionode,gcopy,my_barrier`
- `mult_module, ONLY: symmetrise_mat, mat_p`
- `store_matrix, ONLY: InfoMatrixFile, deallocate_InfoMatrixFile, matrix_store_global`
- `primary_module, ONLY: bundle`
- `input_module, ONLY: io_assign,io_close, leqi`
- `io_module, ONLY: get_file_name`
- `group_module, ONLY: parts`
- `matrix_module, ONLY: matrix_halo`
- `matrix_data, ONLY: halo, max_range`
- `atom_dispenser, ONLY: atom2part, allatom2part`
- `dimens, only: r_super_x, r_super_y, r_super_z`
- `functions, ONLY: heapsort_integer_index`
- `numbers, ONLY: zero`
- `species_module, ONLY: nsf_species, natomf_species, nlpf_species`

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
