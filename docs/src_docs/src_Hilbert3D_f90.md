## Overview
Generates a Hilbert curve for a box of dimension 2**ax by 2**ay by 2**az, where ax, ay and az can be different non-negative integers

## Key Components

### Modules
- `Hilbert3D`: Generates a Hilbert curve for a box of dimension 2**ax by 2**ay by 2**az, where ax, ay and az can be different non-negative integers

### Subroutines
- `Hilbert3D_Initialise(ax, ay, az)`: Initialises Hilbert3D
- `Hilbert3D_IntToCoords(ind, coords)`: Given Hilbert curve step index (counting from 1) as input, outputs the cartesian grid coordinates of the corresponding point in the Hilbert curve.
- `Hilbert3D_CoordsToInt(coords, ind)`: Inverse transorm of Hilbert3D_IntToCoords. Given a cartesian grid coordinate (nx,ny,nz), returns the corresponding step index (counting from 1) of the Hilbert Curve.
- `AllocUnpacked(unpkd, nLLevels, nPLevels, nCLevels)`: Allocates the unpacked index or coordinates
- `DeallocUnpacked(unpkd)`: Deallocates a unpacked type
- `UnpackIndex(ind, unpkdInd)`: Unpacks the step index (start counting from 0) in Hilbert curve into the decomposed set of indices...
- `PackIndex(unpkdInd, ind)`: Packs a type(unpacked) index into an integer step index (counting starts from 0) of the Hilbert curve.
- `UnpackCoords(coords, unpkdCoords)`: Given cartesian coordinates, decomposes the coordinate into the corresponding binary code coordinates...
- `PackCoords(unpkdCoords, coords)`: Inverse transformation of UnpackCoords
- `ChildStartEnd(parentStart, parentEnd, nDims, parentInd, childStart, childEnd)`: Given parent starting position and ending position of a Gray code sequence, and the parent step index, works out child starting position and ending position...
- `TransposeBits(nSrc, src, nDest, dest)`: Given array src of binary numbers each nDest bits long, calculates an array of nDest binary numbers each with nSrc bits in dest...

### Functions
- `GrayEncode(i)` (`integer`): Gray code encoder for canonical reflective Gray code sequence.
- `GrayDecode(gn)` (`integer`): Inverse operation of GrayEncode.
- `GrayEncodeTravel(start, end, nBits, ind)` (`integer`): Rotates the Hilbert curve in the unit cube of nBits dimension...
- `GrayDecodeTravel(start, end, nBits, gn)` (`integer`): Inverse transform of GrayEncodeTravel.
- `MinLen(n, base)` (`integer`): Give the minimum number of digits required to provide a representation of a given integer in a given base

## Important Variables/Constants
**Derived Types:**
- `TYPE unpacked`:
  - `N_l_levels (integer)`: Number of fractal levels in the 1D part of the curve.
  - `N_p_levels (integer)`: Number of fractal levels in the 2D part of the curve.
  - `N_c_levels (integer)`: Number of fractal levels in the 3D part of the curve.
  - `l (integer, dimension(:), pointer)`: Array of indices for the 1D part.
  - `p (integer, dimension(:), pointer)`: Array of indices for the 2D part.
  - `c (integer, dimension(:), pointer)`: Array of indices for the 3D part.

**Key Module Variables:**
- `Alpha (integer, dimension(3))`: Stores `ax, ay, az` (powers of 2 for dimensions) after sorting (longest dimension first).
- `Direction (integer, dimension(3))`: Stores corresponding direction priorities (4,2,1) permuted with Alpha.
- `XYZ (integer, dimension(3))`: Stores original axis indices (1,2,3 for x,y,z) permuted with Alpha.
- `DimLine (integer)`: 2**(Alpha(1) - Alpha(2)), length of the 1D segments.
- `DimSquare (integer)`: 2**(Alpha(2) - Alpha(3)), side length of the 2D square segments.
- `DimCube (integer)`: 2**Alpha(3), side length of the 3D cube segments.
- `NSquaresInLine (integer)`: Alias for DimLine.
- `NCubesInSquare (integer)`: DimSquare**2, number of cubes in a square segment.
- `NPointsInCube (integer)`: DimCube**3, number of points in a cube segment.

## Usage Examples
To be added by developers.

## Dependencies and Interactions

### Used Modules
- `global_module, only: area_init`
- `GenComms, only: cq_abort`
- `memory_module, only: reg_alloc_mem, type_int, reg_dealloc_mem`

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
