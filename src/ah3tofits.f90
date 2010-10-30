program ah3tofits
  ! 
  ! Converts Garrelt's AH3 files fo FITS format
  !
  ! Creates one FITS file per variable
  !
  use ah3mod, only: idebug, ah3_header_t, ah3_grid_t, &
       &            ah3setinputdir, ah3readfile, ah3filename
  use wfitsutils, only: fitswrite
  implicit none
  type(ah3_header_t) :: h
  type(ah3_grid_t) :: g
  integer, parameter :: nv = 6
  character, parameter, dimension(nv) :: id*1 = &
       &(/ 'd', 'u', 'v', 'w', 'p', 'x' /)
  integer :: i
  character :: dateid*8, runid*1, outprefix*15
  integer :: timeid
  character :: rootdir*256 
  ! CGS constants
  real, parameter :: mass_per_particle = 1.67e-24*1.3
  real, parameter :: km_per_sec = 1.e5

  idebug = 1

  print *, 'Root directory?'
  read '(a)', rootdir
  print *, 'Date ID (e.g., 29112005)'
  read '(a)', dateid
  print *, 'Run ID (e.g., a)'
  read '(a)', runid
  print *, 'Time ID (e.g., 70)'
  read *, timeid

  call ah3setinputdir(rootdir)

  call ah3readfile(dateid, runid, timeid, h, g)

  print *, 'maximum density is ', maxval(g%data(:,:,:,1))
  print *, 'at ', maxloc(g%data(:,:,:,1))

  ! convert eden to pressure
  g%data(:,:,:,5) = g%data(:,:,:,5) - &
       & 0.5*(g%data(:,:,:,2)**2 + g%data(:,:,:,3)**2 + g%data(:,:,:,4)**2)&
       & / g%data(:,:,:,1)
  g%data(:,:,:,5) = g%data(:,:,:,5)*(h%gamma - 1.0)


  do i = 2, 4
     ! convert mom fluxes to velocities 
     g%data(:,:,:,i) = g%data(:,:,:,i) / g%data(:,:,:,1)
  end do
  
  ! densities in pcc
  g%data(:,:,:,1) = g%data(:,:,:,1) / mass_per_particle
  ! velocities in km/s
  g%data(:,:,:,2:4) = g%data(:,:,:,2:4) / km_per_sec

  outprefix = ah3filename(dateid, runid, timeid)
  do i = 1, nv
     call fitswrite(real(g%data(:,:,:,i)), outprefix//id(i)//'.fits')
  end do
  
end program ah3tofits
