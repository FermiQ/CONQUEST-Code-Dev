! -*- mode: F90; mode: font-lock -*-
! ------------------------------------------------------------------------------
! $Id$
! ------------------------------------------------------------------------------
! Module auxiliary_types
! ------------------------------------------------------------------------------
! Code area : 
! ------------------------------------------------------------------------------

!****h* Conquest/auxiliary_types
! NAME
!  auxiliary_types
! PURPOSE
!  This module defines auxiliary data types used in the Conquest code.
! AUTHOR
!  M.Arita
! CREATION DATE
!  2013/12/05
! MODIFICATION HISTORY
! SOURCE
!
module auxiliary_types
  ! Use statement for the datatypes module.  Presumably defines basic data types used here.
  use datatypes
  implicit none

  !****s* auxiliary_types/group_aux
  ! NAME
  !  group_aux
  ! PURPOSE
  !  This derived type defines all variables related to auxiliary groups of atoms.  This likely facilitates grouping atoms for specific calculations or analysis.
  ! AUTHOR
  ! SOURCE
  !
  type group_aux
   ! Scalars:  These variables store single values.
   integer :: n_grp                        ! No. of groups defined.
   character(20) :: filename               ! Name of the file containing auxiliary group information.
   ! Arrays: These variables are arrays, the size of which is determined dynamically.  The use of pointers allows for flexible memory allocation.
   character(20),pointer :: grp_name(:)    ! Array of group names (strings of length 20).
   integer,pointer :: n_atom_in_grp(:)     ! Array containing the number of atoms in each group.
   integer,pointer :: n_subgrp(:)          ! Array containing the number of subgroups within each group.
   integer,pointer :: iatom_beg(:)         ! Array indicating the starting index of atoms for each subgroup within the glob_atom array.
   integer,pointer :: ibeg_grp(:)          ! Array indicating the starting index of atoms for each group within the glob_atom array.
   integer,pointer :: glob_atom(:)         ! Array containing the global indices of all atoms belonging to the groups.
  end type group_aux
  
end module auxiliary_types