## Overview
Provides the interface to LibXC routines along with the complete Conquest
library of XC functionals.  This means that any changes to XC_CQ_module.f90
need to be duplicated in this module but it simplifies the overall source
code and removes one layer of subroutine calls.

## Key Components

### Modules
- `XC`: Provides the interface to LibXC routines and Conquest native XC functionals.

### Subroutines
- `init_xc()`: Initialises the exchange-correlation calculations (can use LibXC or Conquest native).
- `write_xc_refs()`: Write XC references (primarily for LibXC).
- `get_xc_potential(density, xc_potential, xc_epsilon, xc_energy, size, x_energy)`: Interface to LibXC or CQ routines for XC potential.
- `get_dxc_potential(density, dxc_potential, nsize, density_out)`: Interface to LibXC or CQ routines for derivative of XC potential.
- `get_libxc_potential(density, xc_potential, xc_epsilon, xc_energy, size, x_energy)`: Calculates XC potential using LibXC.
- `get_libxc_dpotential(density, dxc_potential, size, density_out)`: Calculates derivative of XC potential using LibXC.
- `get_xc_energy(density, xc_energy, size)`: Interface to LibXC or CQ routines for XC energy.
- `get_libxc_energy(density, xc_energy, size)`: Calculates XC energy using LibXC.
- `get_xc_potential_LDA_PZ81(density, xc_potential, xc_epsilon, xc_energy, size, x_epsilon, c_epsilon, x_energy)`: Calculates the exchange-correlation potential (LDA PZ81).
- `get_GTH_xc_potential(density, xc_potential, xc_epsilon, xc_energy, size)`: Calculates the exchange-correlation potential (LDA GTH).
- `get_xc_potential_LSDA_PW92(density, xc_potential, xc_epsilon, xc_energy_total, size, x_energy_total)`: Calculates the spin polarized exchange-correlation potential (LSDA PW92).
- `Vxc_of_r_LSDA_PW92(nspin, rho_r, eps_x, eps_c, Vx, Vc)`: Calculates the exchange-correlation energy density and potential as a function of rho(r) (LSDA PW92).
- `get_xc_potential_GGA_PBE(density, xc_potential, xc_epsilon, xc_energy, grid_size, flavour, x_energy)`: Calculates the exchange-correlation potential (GGA PBE and variants).
- `get_xc_potential_hyb_PBE0(density, xc_potential, exx_a, xc_epsilon, xc_energy, grid_size, flavour, x_energy)`: Work routine for hybrid PBE0 XC potential.
- `get_xc_energy_LDA_PZ81(density, xc_energy, size)`: Calculates the exchange-correlation energy (LDA PZ81).
- `get_GTH_xc_energy(density, xc_energy, size)`: Calculates the exchange-correlation energy (LDA GTH).
- `get_xc_energy_LSDA_PW92(density, xc_energy_total, size)`: Calculates the spin polarized exchange-correlation energy (LSDA PW92).
- `get_xc_energy_GGA_PBE(density, xc_energy, grid_size, flavour)`: Calculates the exchange-correlation energy (GGA PBE and variants).
- `get_xc_energy_hyb_PBE0(density, xc_energy, grid_size, exx_a, flavour)`: Work routine for hybrid PBE0 XC energy.
- `build_gradient(density, grad_density, size)`: Calculates the gradient of the density.
- `get_dxc_potential_LDA_PZ81(density, dxc_potential, size)`: Calculates the derivative of the exchange-correlation potential (LDA PZ81).
- `get_GTH_dxc_potential(density, dxc_potential, size)`: Calculates the derivative of the exchange-correlation potential (LDA GTH).
- `get_dxc_potential_LSDA_PW92(density, dxc_potential_ddensity, size, eclda, declda_drho, d2eclda_drho2)`: Calculates the derivative of the exchange-correlation potential (LSDA PW92).
- `get_dxc_potential_GGA_PBE(density, density_out, dxc_potential, size, flavour)`: Calculates the potential needed for the non-self-consistent within GGA using PBE.
- `eps_xc_of_r_GGA_PBE(nspin, flavour, rho_r, grho_r, eps_x, eps_c, drhoEps_x, drhoEps_c)`: Calculates the exchange and correlation energy density and its derivatives for the PBE functional.

### Functions
- `PW92_G(rs, A, alpha1, beta1, beta2, beta3, beta4, p, G, dG_drs, d2G_drs2)` (`real(double)`): Calculates the interpolation function G defined in PW92 LSDA paper.

## Important Variables/Constants
- `XC_GGA_stress (real(double), dimension(3,3), public)`: Stress tensor contribution from GGA XC functional.
- `s_6 (real(double), public)`: Parameter for DFT-D2 dispersion correction.
- `flag_is_GGA (logical, public)`: True if the selected functional is a GGA.
- `flag_functional_type (integer, public)`: Numerical flag choosing functional type (negative for LibXC).
- `flag_different_functional (logical, public)`: True if using a different functional than pseudopotentials.
- `n_xc_terms (integer)`: Number of LibXC terms (1 for combined, 2 for separate X and C).
- `i_xc_family (integer, dimension(2))`: Family of functional for LibXC terms (e.g., LDA, GGA).
- `xc_func (type(xc_f90_pointer_t), dimension(:), allocatable)`: Pointer to LibXC functional.
- `xc_info (type(xc_f90_pointer_t), dimension(:), allocatable)`: Pointer to LibXC functional info.
- `flag_use_libxc (logical)`: True if LibXC is being used.
- `functional_description (character(len=15))`: String description of the selected functional.

## Usage Examples
To be added by developers.

## Dependencies and Interactions

### Used Modules
- `datatypes`
- `global_module, only: area_ops, io_lun, iprint_ops, spin_factor, nspin, flag_dft_d2`
- `xc_f90_types_m`
- `xc_f90_lib_m`
- `GenComms, only: inode, ionode, cq_abort, cq_warn, gsum`
- `numbers`
- `dimens, only: grid_point_volume, n_my_grid_points`
- `fft_module, only: fft3, recip_vector`
- `memory_module, only: reg_alloc_mem, reg_dealloc_mem, type_dbl`

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
