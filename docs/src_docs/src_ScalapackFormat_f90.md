## Overview
Within Conquest and ParaDens, we want to introduce parallel   
diagonalisation using ScaLAPACK.  This small module contains
subroutines to create the different formats required: reference 
(in other words the "unpacked" or global ScaLAPACK matrix), Scalapack 
(the matrix as distributed over processors for ScaLAPACK)   
and the relationship to Conquest primary sets and atom      
numbers.

## Key Components

### Modules
- `ScalapackFormat`: Handles transformations between Conquest data storage and ScaLAPACK's 2D block cyclic distribution.

### Subroutines
- `allocate_arrays(nkp)`: Allocates storage for Scalapack format arrays
- `deallocate_arrays()`: Deallocates the memory allocated by allocate_arrays
- `pg_initialise(nkp)`: Distribute the processes and kpoints into proc_groups.
- `ref_to_SC_blocks()`: Takes blocks allocated to processor grid in reference format and converts to compacted Scalapack blocks
- `make_maps()`: Creates maps between blocks on processors and reference format.
- `find_SC_row_atoms()`: Distributes atoms over rows in Scalapack format according to Conquest processor/primary atom ordering.
- `check_block_parameters(flag_err)`: [Add description - nested routine for parameter check]
- `find_ref_row_atoms()`: Given the Scalapack format listing for atoms corresponding to each row, generate the reference format listing...
- `find_SC_col_atoms()`: Given the atoms corresponding to the columns in the reference format, generate the atoms corresponding to the columns in Scalapack format.

### Functions
None found in this file.

## Important Variables/Constants
**Derived Types:**
- `TYPE block_atom`: Stores data for atoms in Scalapack/reference/Conquest transformation arrays.
  - `part (integer)`: Partition label.
  - `atom (integer)`: Atom sequence number in partition.
  - `support_fn (integer)`: Support function index.
- `TYPE processor_data`: Contains data about processors and where their data lies.
  - `block (integer)`: Starting block index.
  - `atom (integer)`: Starting atom index within block.
  - `rows (integer)`: Number of rows on this processor.
  - `cols (integer)`: Number of columns on this processor.
  - `startrow (integer)`: Global starting row index for this processor's data.
- `TYPE CC_to_SC_map`: Maps from Fundamental Simulation Cell (CC) partition/sequence to ScaLAPACK column block/atom labelling.
  - `block_c (integer)`: ScaLAPACK block column index.
  - `row_c (integer)`: Row index within the ScaLAPACK column block.
  - `block_r (integer)`: ScaLAPACK block row index.
  - `row_r (integer)`: Row index within the ScaLAPACK row block.
- `TYPE CQ_to_SC_map`: Gives for each row in matrix the Conquest indexing labels.
  - `support_fn (integer)`: Support function index.
  - `atom (integer)`: Atom sequence number in partition.
  - `partition (integer)`: Partition sequence number on its processor.
  - `CClabel (integer)`: Global Conquest CCarbon (partition) label.

**Key Module Variables (selection):**
- `matrix_size (integer)`: Dimension of the square matrix.
- `flag_padH (logical)`: Flag indicating if H (and S) matrices are padded.
- `matrix_size_padH (integer)`: Dimension of the padded matrix.
- `block_size_r, block_size_c (integer)`: Row and column block sizes for ScaLAPACK.
- `proc_groups (integer)`: Total number of k-point process-groups.
- `pgid (integer)`: Local process-group ID for the current process.
- `proc_rows, proc_cols (integer)`: Dimensions of the processor grid.
- `CC_to_SC (type(CC_to_SC_map), allocatable, dimension(:,:,:))`: Map from CC labels to ScaLAPACK block/row indices.
- `CQ2SC_row_info (type(CQ_to_SC_map), allocatable, dimension(:))`: Information about Conquest atom for each matrix row.
- `procid (integer, allocatable, dimension(:,:,:))`: Maps (proc_group, proc_row, proc_col) to global processor ID.
- `mapx, mapy (integer, allocatable, dimension(:,:,:))`: Maps (processor, local_row_block, local_col_block) to global reference block indices.

## Usage Examples
To be added by developers.

## Dependencies and Interactions

### Used Modules
- `global_module, ONLY: io_lun, area_DM, iprint_DM, numprocs`
- `timer_module, only: start_timer, stop_timer`
- `timer_stdclocks_module, only: tmr_std_allocation`
- `datatypes, ONLY: double`
- `GenComms, ONLY: cq_abort, myid, inode`
- `numbers, ONLY: RD_ERR`
- `group_module, ONLY: parts`
- `species_module, ONLY: nsf_species, species`

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
