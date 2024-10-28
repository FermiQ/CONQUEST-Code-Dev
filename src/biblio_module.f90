!!****h* Conquest/biblio *
!!  NAME
!!   biblio_module
!!  PURPOSE
!!   Generate a relevant bibliography based on input flags
!!  AUTHOR
!!   Zamaan Raza
!!  CREATION DATE
!!   2019/07/03
!!  MODIFICATION HISTORY
!!   2020/01/03 12:14 dave
!!    Added check in get_ref to catch non-existent references
!!  SOURCE
!!
module biblio

   use global_module,  only: io_lun
   use GenComms,       only: inode, ionode
 
   implicit none
 
   ! File name for the BibTeX bibliography file.
   character(len=*), parameter :: bibtex_file = "conquest.bib"
   ! Format string for writing reference information.
   character(len=*), parameter :: ref_fmt = '(4x,a," ",i0,", ",i0," (",i0,")")'
   ! Format string for writing DOI information.
   character(len=*), parameter :: doi_fmt = '(4x,"http://dx.doi.org/",a)'
   ! Format string for writing the BibTeX key.
   character(len=*), parameter :: bib_key_fmt = '("@article{",a,",")'
   ! Format string for writing BibTeX fields with string values.
   character(len=*), parameter :: bib_afield_fmt = '(2x,a," = {{",a,"}},")'
   ! Format string for writing BibTeX fields with integer values.
   character(len=*), parameter :: bib_ifield_fmt = '(2x,a," = {",i0,"},")'
   ! Maximum number of references allowed.
   integer, parameter          :: max_refs = 50
   ! Flag to indicate whether to dump the bibliography.
   logical                     :: flag_dump_bib
 
   ! Structure to hold information about a single reference.
   type type_reference
 
      ! BibTeX key for the reference.
      character(len=40)   :: key
      ! Authors of the reference.
      character(len=400)  :: authors
      ! Title of the reference.
      character(len=400)  :: title
      ! Journal name for the reference.
      character(len=400)  :: journal
      ! Volume number of the reference.
      integer             :: volume
      ! Page number(s) of the reference.
      integer             :: page
      ! Year of publication of the reference.
      integer             :: year
      ! DOI of the reference.
      character(len=80)   :: doi
      ! Any comments associated with the reference.
      character(len=132)   :: comment
 
    contains
 
      ! Procedure to cite a reference.
      procedure, public :: cite_reference
      ! Procedure to write a BibTeX record for a reference.
      procedure, public :: write_bib
 
   end type type_reference
 
   ! Structure to manage a collection of references.
   type type_bibliography
 
      ! Array of references.
      type(type_reference), allocatable, dimension(:) :: db
      ! Number of references currently stored.
      integer             :: nrefs
      ! Flag to indicate whether it's the first reference.
      logical             :: first
 
    contains
 
      ! Procedure to initialize a bibliography.
      procedure, public :: init_bib
      ! Procedure to close a bibliography.
      procedure, public :: close_bib
      ! Procedure to add a reference to the bibliography.
      procedure, public :: add_ref
      ! Procedure to retrieve a reference by key.
      procedure, public :: get_ref
      ! Procedure to cite a reference.
      procedure, public :: cite
 
   end type type_bibliography
 
 contains
 
   !!****f* biblio/cite_reference *
   !!
   !!  NAME 
   !!   cite_reference
   !!  PURPOSE
   !!   Cite a reference in Conquest_out
   !!  AUTHOR
   !!   Zamaan Raza
   !!  CREATION DATE
   !!   2019/07/04
   !!  MODIFICATION HISTORY
   !!
   !!  SOURCE
   !!
   subroutine cite_reference(ref)
 
     ! Reference to be cited.
     class(type_reference) :: ref
 
     ! Only write to the output file if this is the main node.
     if (inode==ionode) then
        ! Write the comment, title, and authors to the output file.
        write(io_lun,'(4x,a)') trim(ref%comment)
        write(io_lun,'(4x,a)') trim(ref%title)
        write(io_lun,'(4x,a)') trim(ref%authors)
        ! Write the journal, volume, page, and year to the output file.
        write(io_lun,ref_fmt) trim(ref%journal), ref%volume, ref%page, ref%year
        ! Write the DOI to the output file.
        write(io_lun,doi_fmt) trim(ref%doi)
        ! Add a blank line to the output file.
        write(io_lun,*)
     end if
 
   end subroutine cite_reference
   !!***
 
   !!****f* biblio/write_bib *
   !!
   !!  NAME 
   !!   write_bib
   !!  PURPOSE
   !!   Generate a BibTeX record as a string
   !!  AUTHOR
   !!   Zamaan Raza
   !!  CREATION DATE
   !!   2019/07/04
   !!  MODIFICATION HISTORY
   !!
   !!  SOURCE
   !!
   subroutine write_bib(ref, first)
 
     use io_module, only: io_assign, io_close
 
     ! Reference to write to the BibTeX file.
     class(type_reference), intent(in) :: ref
     ! Flag indicating whether this is the first reference.
     logical, intent(inout)            :: first
 
     ! Local unit number for file I/O.
     integer :: lun
     ! String to hold the BibTeX record.
     character(len=400)                :: str
 
    ! Only write to the BibTeX file if this is the main node.
    if(inode == ionode) then
     ! Assign a local unit number.
     call io_assign(lun) 
     ! Open the BibTeX file for writing or appending.
     if (first) then
        open(unit=lun,file=bibtex_file,status='replace')
        ! Set the first flag to false after the first write.
        first = .false.
     else
        open(unit=lun,file=bibtex_file,position='append')
     end if
 
     ! Write the BibTeX record to the file.
     write(lun,bib_key_fmt) trim(ref%key)
     write(lun,bib_afield_fmt) "author", trim(ref%authors)
     write(lun,bib_afield_fmt) "title", trim(ref%title)
     write(lun,bib_afield_fmt) "journal", trim(ref%journal)
     write(lun,bib_ifield_fmt) "year", ref%year
     write(lun,bib_ifield_fmt) "volume", ref%volume
     write(lun,bib_ifield_fmt) "pages", ref%page
     write(lun,bib_afield_fmt) "doi", trim(ref%doi)
     write(lun,'("}")')
 
     ! Close the BibTeX file.
     call io_close(lun)
 
    endif !(inode == ionode) 
 
   end subroutine write_bib
   !!***
 
   ! Function to retrieve a reference from the bibliography by key.
   type(type_reference) function get_ref(bib, key)
 
     use input_module,  only: leqi
     use GenComms,      only: cq_abort
 
     ! Bibliography to search.
     class(type_bibliography), intent(inout) :: bib
     ! Key of the reference to retrieve.
     character(*), intent(in)                :: key
 
     ! Loop counter and flag to indicate whether the reference was found.
     integer       :: i, done
     ! Temporary variable to hold the key.
     character(40) :: k
 
     ! Copy the key to a local variable.
     k = key
 
     ! Initialize the done flag.
     done = 0
     ! Loop through the references in the bibliography.
     do i=1,max_refs
        ! Check if the key matches.
        if (leqi(bib%db(i)%key, k)) then
           ! Return the reference if found.
           get_ref = bib%db(i)
           ! Set the done flag.
           done = 1
           ! Exit the loop.
           exit
        end if
     end do
     ! Abort if the reference is not found.
     if(done==0) call cq_abort("Unable to find reference key "//key)
   end function get_ref
 
   ! Subroutine to initialize a bibliography.
   subroutine init_bib(bib)
 
     ! Bibliography to initialize.
     class(type_bibliography), intent(inout) :: bib
 
     ! Loop counter.
     integer :: i
 
     ! Allocate memory for the reference array.
     allocate(bib%db(max_refs))
     ! Initialize the number of references and the first flag.
     bib%nrefs = 0
     bib%first = .true.
 
   end subroutine init_bib
 
   ! Subroutine to deallocate memory for a bibliography.
   subroutine close_bib(bib)
 
     ! Bibliography to close.
     class(type_bibliography), intent(inout) :: bib
 
     ! Loop counter.
     integer :: i
 
     ! Deallocate memory for the reference array.
     deallocate(bib%db)
 
   end subroutine close_bib
 
   !!****f* biblio/add_ref *
   !!
   !!  NAME 
   !!   add_ref
   !!  PURPOSE
   !!   Add a reference
   !!  AUTHOR
   !!   Zamaan Raza
   !!  CREATION DATE
   !!   2019/07/04
   !!  MODIFICATION HISTORY
   !!
   !!  SOURCE
   !!
   subroutine add_ref(bib, key, authors, title, journal, volume, page, &
        year, doi, comment)
 
     use input_module, only: leqi
     use GenComms,     only: cq_abort
 
     ! Bibliography to add the reference to.
     class(type_bibliography), intent(inout) :: bib
     ! Key for the new reference.
     character(*), intent(in)  :: key
     ! Authors of the new reference.
     character(*), intent(in)  :: authors
     ! Title of the new reference.
     character(*), intent(in)  :: title
     ! Journal of the new reference.
     character(*), intent(in)  :: journal
     ! Volume of the new reference.
     integer, intent(in)       :: volume
     ! Page of the new reference.
     integer, intent(in)       :: page
     ! Year of the new reference.
     integer, intent(in)       :: year
     ! DOI of the new reference.
     character(*), intent(in)  :: doi
     ! Comment for the new reference.
     character(*), intent(in)  :: comment
 
     ! Variable to hold the new reference.
     type(type_reference), allocatable, target :: reference
 
     ! Increment the reference counter.
     bib%nrefs = bib%nrefs + 1
     ! Abort if the maximum number of references is exceeded.
     if (bib%nrefs > max_refs) &
          call cq_abort("Number of references > max_refs: ", bib%nrefs, max_refs)
 
     ! Assign values to the new reference.
     bib%db(bib%nrefs)%key = key
     bib%db(bib%nrefs)%authors = authors
     bib%db(bib%nrefs)%title = title
     bib%db(bib%nrefs)%journal = journal
     bib%db(bib%nrefs)%volume = volume
     bib%db(bib%nrefs)%page = page
     bib%db(bib%nrefs)%year = year
     bib%db(bib%nrefs)%doi = doi
     bib%db(bib%nrefs)%comment = comment
 
   end subroutine add_ref
   !!***
 
   !!****f* biblio/cite *
   !!
   !!  NAME 
   !!   cite
   !!  PURPOSE
   !!   Write reference to output file and bibliography
   !!  AUTHOR
   !!   Zamaan Raza
   !!  CREATION DATE
   !!   2019/07/04
   !!  MODIFICATION HISTORY
   !!   2020/03/24 14:22 dave
   !!    Changed to write reference key only if iprint<2
   !!   2022/09/21 10:58 dave
   !!    Changed to write reference key only if iprint<4
   !!  SOURCE
   !!
   subroutine cite(bib, key, punc, pre)
 
     use global_module, ONLY: iprint_init
     ! Bibliography containing the reference.
     class(type_bibliography), intent(inout) :: bib
     ! Key of the reference to cite.
     character(*), intent(in)                :: key
     ! Optional punctuation to append to the citation.
     character(2), intent(in), optional      :: punc
     ! Optional prefix to add before the citation.
     character(*), intent(in), optional      :: pre
 
     ! Variable to hold the retrieved reference.
     type(type_reference)                    :: reference
     ! Variable to hold the prefix string.
     character(20) :: start
 
     ! Retrieve the reference from the bibliography.
     reference = bib%get_ref(key)
     ! Write the full reference or just the key based on iprint_init value.
     if(iprint_init>3) then
        ! Write the full reference if iprint_init > 3.
        call reference%cite_reference
        ! Write the BibTeX record if flag_dump_bib is true.
        if (flag_dump_bib) call reference%write_bib(bib%first)
     else
        ! Write only the key if iprint_init <= 3.
        ! Add prefix if provided.
        if(present(pre)) then
           write(start,'(a)') pre
           ! Add punctuation if provided.
           if(present(punc)) then
              write(io_lun,fmt='(4x,a18,a,a2)',advance='no') adjustl(start), trim(reference%key), punc
           else
              write(io_lun,fmt='(4x,a18,a)',advance='no') adjustl(start), trim(reference%key)
           end if
        else
           ! Add punctuation if provided.
           if(present(punc)) then
              write(io_lun,fmt='(a,a2)',advance='no') trim(reference%key),punc
           else
              write(io_lun,fmt='(a)',advance='no') trim(reference%key)
           end if
        end if
        ! Write the BibTeX record if flag_dump_bib is true.
        call reference%write_bib(bib%first)
     end if
 
   end subroutine cite
   !!***
 
 end module biblio