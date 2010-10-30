module ah3mod
  implicit none
  !!
  !! ah3mod - read/write Garrelt's AH3D output format
  !!
  !! 29 Nov 2005 (WJH)
  !!

  !! NOTE: We assume all data is 3D

  integer, parameter :: DP = kind(1.0d0) ! default double
  integer :: idebug = 1 ! set to 0 for silent operation
  integer :: neuler

  character(len=256) :: inputdir

  type ah3_header_t
     character(len=80) :: banner
     integer :: nrOfDim, nrOfVars, nrOfGrids, refinementFactor, frameNr
     real(kind=DP) :: gamma, time
  end type ah3_header_t
  
  type ah3_grid_t
     integer, dimension(3) :: n
     real(kind=DP), dimension(3) :: corners, cellSizes
     integer :: level
     ! Simpler to just use have an array in here directly 
     ! WARNING This is not strictly valid F95 but works on ifort >= 8
     real(kind=DP), allocatable, dimension(:,:,:,:)  :: data
  end type ah3_grid_t
  
  

!! Earlier idea to have another level of structure - abandoned 29 Nov 2005
!!$  type ah3_cells_t
!!$     real(kind=DP), allocatable, dimension(:,:,:)  :: &
!!$          & rho, rhov1, rhov2, rhov3, rhoe 
!!$     real(kind=DP), allocatable, dimension(:,:,:,:)  :: vars
!!$  end type ah3_cells_t

contains
  
  subroutine ah3setinputdir(dir)
    character(len=*), intent(in) :: dir
    inputdir = dir
  end subroutine ah3setinputdir

  function ah3prefix(revdate, runid) result(prefix)
    character, intent(in) :: revdate*8, runid*1
    character(len=10) :: prefix
    write(prefix, '(a8,"_",a1)') revdate, runid
  end function ah3prefix

  function ah3filename(revdate, runid, nframe) result(name)
    character, intent(in) :: revdate*8, runid*1
    integer, intent(in) :: nframe
    character(len=15) :: name
    write(name, '(a8,"_",a1,"_",i4.4)') revdate, runid, nframe
  end function ah3filename
  
  subroutine ah3readfile(&
       & revdate, runid, nframe, &
       & hdr, grid &
       &)

    character, intent(in) :: revdate*8, runid*1
    integer, intent(in) :: nframe

    type(ah3_header_t), intent(out) :: hdr
    type(ah3_grid_t), intent(out) :: grid

    integer :: iv, ierr=0
    character :: filename*19
    namelist / hdr_namelist / hdr

    filename = ah3filename(revdate, runid, nframe)//".ah3"

    if (idebug>0) print *, 'Reading from AH3D file ', filename

    open(1, file=trim(inputdir)//'/'//filename, &
         &  action='read', form='unformatted')

    read(1) hdr%banner
    read(1) hdr%nrOfDim
    read(1) hdr%nrOfVars
    read(1) hdr%nrOfGrids
    read(1) hdr%refinementFactor
    read(1) hdr%frameNr
    read(1) hdr%gamma
    read(1) hdr%time

    neuler = 2 + hdr%nrOfDim

    if (idebug>0) write(*, hdr_namelist)

    read(1) grid%n
    read(1) grid%corners
    read(1) grid%cellSizes
    read(1) grid%level
    if (idebug>0) print *, grid%n, grid%corners, grid%cellSizes, grid%level
    allocate (grid%data(grid%n(1), grid%n(2), grid%n(3), hdr%nrOfVars))
    do iv = 1, neuler
       print '(a,i0)', 'Reading variable #', iv
       read(1,iostat=ierr) grid%data(:,:,:,iv)
       if (ierr/=0) print *, 'IO error = ', ierr
    end do
    if (hdr%nrOfVars > neuler) then
       print '(a,i0,a,i0)', &
            & 'Reading variables #', neuler+1, ' to #', hdr%nrOfVars 
       read(1,iostat=ierr) grid%data(:,:,:,neuler+1:hdr%nrOfVars)
       if (ierr/=0) print *, 'IO error = ', ierr
    endif
    close(1)

  end subroutine ah3readfile
  
  subroutine ah3writefile(&
       & revdate, runid, nframe, &
       & hdr, grid &
       &)

    character, intent(in) :: revdate*8, runid*1
    integer, intent(in) :: nframe

    type(ah3_header_t), intent(in) :: hdr
    type(ah3_grid_t), intent(in) :: grid

    integer :: iv
    character :: filename*19
    namelist / hdr_namelist / hdr

    filename = ah3filename(revdate, runid, nframe)//".ah3"

    if (idebug>0) print *, 'Writing to AH3D file ', filename

    open(1, file=trim(inputdir)//'/'//filename, &
         &  action='write', form='unformatted')

    write(1) hdr%banner
    write(1) hdr%nrOfDim
    write(1) hdr%nrOfVars
    write(1) hdr%nrOfGrids
    write(1) hdr%refinementFactor
    write(1) hdr%frameNr
    write(1) hdr%gamma
    write(1) hdr%time

    close(1)

    if (idebug>0) write(*, hdr_namelist)

    open(1, file=trim(inputdir)//'/'//filename, &
         &  action='readwrite', form='unformatted', position='append')
    write(1) grid%n
    write(1) grid%corners
    write(1) grid%cellSizes
    write(1) grid%level
    if (idebug>0) print *, grid%n, grid%corners, grid%cellSizes, grid%level
    close(1)

    open(1, file=trim(inputdir)//'/'//filename, &
         &  action='readwrite', form='unformatted', position='append')
    do iv = 1, neuler
       write(1) grid%data(:,:,:,iv)
    end do
    if (hdr%nrOfVars > neuler) then
       write(1) grid%data(:,:,:,neuler+1:hdr%nrOfVars)
    endif
    
    close(1)

  end subroutine ah3writefile
  


  !! Garrelt's original description:

  ! AH3D output format (24-10-2002)

  ! File name: 24102002_a_0000.ah3

  ! Header:
  ! string 80 bytes
  ! int    nrOfDim
  ! int    nrOfVars
  ! int    nrOfGrids
  ! int    refinementFactor
  ! int    frameNr
  ! double gamma
  ! double time

  ! Grids:
  ! int    nrOfCells1 [, nrOfCells2, nrOfCells3]
  ! double corner1 [, corner2, corner3]
  ! double cellSize1 [,cellSize2, cellSize3]
  ! int    level

  ! Cells;
  ! double rho
  ! double rho*v1 [, rho*v1, rho*v3]
  ! double rho*e
  ! [double var(nrOfVars-(2+nrOfDim))]



end module ah3mod
