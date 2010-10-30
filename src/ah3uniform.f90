program ah3uniform
  ! takes an ah3 file and makes it uniform
  use ah3mod
  implicit none
  type(ah3_header_t) :: h
  type(ah3_grid_t) :: g
  integer :: npix
  real(kind=DP) :: avdensity, avenergy
  
  character :: dateid*8, runid*1, indir*256, outprefix*15, prefix*10
  integer :: timeid

  print *, 'Date ID (e.g., 29112005)'
  read '(a)', dateid
  print *, 'Run ID (e.g., a)'
  read '(a)', runid
  print *, 'Time ID (e.g., 70)'
  read *, timeid

  call ah3setinputdir('./')     !just use local directory

  call ah3readfile(dateid, runid, timeid, h, g)

  ! subtract off the kinetic energy 
  g%data(:,:,:,5) = g%data(:,:,:,5) - &
       & 0.5*(g%data(:,:,:,2)**2 + g%data(:,:,:,3)**2 + g%data(:,:,:,4)**2)/g%data(:,:,:,1)

  npix = product(g%n)
  avdensity = sum(g%data(:,:,:,1))/real(npix, DP)
  avenergy = sum(g%data(:,:,:,5))/real(npix, DP)
  
  print *, 'average density is ', avdensity
  print *, 'average energy is ', avenergy

  ! reset the grid to homogeneous and static
  g%data(:,:,:,1) = avdensity
  g%data(:,:,:,5) = avenergy
  g%data(:,:,:,2) = 0.0_dp
  g%data(:,:,:,3) = 0.0_dp
  g%data(:,:,:,4) = 0.0_dp

  print *, 'New run ID (e.g., x)'
  read '(a)', runid

  call ah3writefile(dateid, runid, timeid, h, g)
 


end program ah3uniform
