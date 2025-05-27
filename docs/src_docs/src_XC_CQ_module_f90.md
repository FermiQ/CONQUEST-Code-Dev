## Overview
Collects together all the routines associated to
exchange-correlation functionals (note the van der Waal
implementations are in its separate module)

This version is now Conquest native implementations of
the functionals (as opposed to the LibXC interface)

## Key Components

### Modules
- `XC`: Collects together all the routines associated to exchange-correlation functionals.

### Subroutines
- `init_xc()`: Initialises the exchange-correlation calculations
- `write_xc_refs()`: Blank subroutine; with LibXC, write XC references
- `get_xc_potential(density, xc_potential, xc_epsilon, xc_energy, size, x_energy)`: Interface to CQ routines for XC potential.
- `get_dxc_potential(density, dxc_potential, nsize, density_out)`: Interface to CQ routines for derivative of XC potential.
- `get_xc_potential_LDA_PZ81(density, xc_potential, xc_epsilon, xc_energy, size, x_epsilon, c_epsilon, x_energy)`: Calculates the exchange-correlation potential (LDA PZ81).
- `get_GTH_xc_potential(density, xc_potential, xc_epsilon, xc_energy, size)`: Calculates the exchange-correlation potential (LDA GTH).
- `get_xc_potential_LSDA_PW92(density, xc_potential, xc_epsilon, xc_energy_total, size, x_energy_total)`: Calculates the spin polarized exchange-correlation potential (LSDA PW92).
- `Vxc_of_r_LSDA_PW92(nspin, rho_r, eps_x, eps_c, Vx, Vc)`: Calculates the exchange-correlation energy density and potential as a function of rho(r) (LSDA PW92).
- `get_xc_potential_GGA_PBE(density, xc_potential, xc_epsilon, xc_energy, grid_size, flavour, x_energy)`: Calculates the exchange-correlation potential (GGA PBE and variants).
- `get_xc_potential_hyb_PBE0(density, xc_potential, exx_a, xc_epsilon, xc_energy, grid_size, flavour, x_energy)`: Work routine for hybrid PBE0 XC potential.
- `get_xc_energy(density, xc_energy, size)`: Interface to CQ routines for XC energy.
- `get_xc_energy_LDA_PZ81(density, xc_energy, size)`: Calculates the exchange-correlation energy (LDA PZ81).
- `get_GTH_xc_energy(density, xc_energy, size)`: Calculates the exchange-correlation energy (LDA GTH).
- `get_xc_energy_LSDA_PW92(density, xc_energy_total, size)`: Calculates the spin polarized exchange-correlation energy (LSDA PW92).
- `get_xc_energy_GGA_PBE(density, xc_energy, grid_size, flavour)`: Calculates the exchange-correlation energy (GGA PBE and variants).
- `get_xc_energy_hyb_PBE0(density, xc_energy, grid_size, exx_a, flavour)`: Work routine for hybrid PBE0 XC energy.
- `build_gradient(density, grad_density, size)`: Calculates the gradient of the density, and its modulus.
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
- `flag_functional_type (integer, public)`: Numerical flag choosing functional type.
- `flag_different_functional (logical, public)`: True if using a different functional than pseudopotentials.
- `functional_lda_pz81 (integer, parameter)`: Identifier for LDA PZ81 functional.
- `functional_lda_gth96 (integer, parameter)`: Identifier for LDA GTH96 functional.
- `functional_lda_pw92 (integer, parameter)`: Identifier for LDA PW92 functional.
- `functional_gga_pbe96 (integer, parameter)`: Identifier for GGA PBE96 functional.
- `functional_hyb_pbe0 (integer, parameter)`: Identifier for hybrid PBE0 functional.
- `functional_description (character(len=15))`: String description of the selected functional.

## Usage Examples
To be added by developers.

## Dependencies and Interactions

### Used Modules
- `datatypes`
- `global_module, only: area_ops, io_lun, iprint_ops, spin_factor, flag_dft_d2, nspin, flag_stress, flag_full_stress`
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
