program ah3rebin
  ! Rebin an AH3 file to half the resolution
  use ah3mod
  implicit none
  type(ah3_header_t) :: h
  type(ah3_grid_t) :: g, gg
  integer :: i, j, k, m

  character :: dateid*8, runid*1, newrunid*1
  integer :: timeid

  print *, 'Date ID (e.g., 29112005)'
  read '(a)', dateid
  print *, 'Run ID (e.g., a)'
  read '(a)', runid
  print *, 'Time ID (e.g., 70)'
  read *, timeid

  call ah3setinputdir('./')     !just use local directory

  call ah3readfile(dateid, runid, timeid, h, g)
 
  print *, 'maximum density is ', maxval(g%data(:,:,:,1))
  print *, 'at ', maxloc(g%data(:,:,:,1))

  ! new grid is half the size
  gg%n = g%n/2
  gg%corners = gg%corners
  gg%cellSizes = 2.0*g%cellSizes
  gg%level = g%level
  allocate(gg%data(gg%n(1), gg%n(2), gg%n(3), h%nrOfVars))

  forall(i=1:gg%n(1), j=1:gg%n(2), k=1:gg%n(3), m=1:h%nrOfVars)
     gg%data(i, j, k, m) = sum(g%data(2*i-1:2*i, 2*j-1:2*j, 2*k-1:2*k, m))/8.0
  end forall
  

  print *, 'New run ID (e.g., y)'
  read '(a)', newrunid

  if (newrunid == runid) then
     print *, 'Warning: attempt to overwrite data ', dateid, runid
     print *, 'Aborting'
     stop
  end if
  
  call ah3writefile(dateid, newrunid, timeid, h, gg)
 

end program ah3rebin
