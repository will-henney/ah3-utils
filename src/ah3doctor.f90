program ah3doctor
  ! remove some mass from the center of the grid
  use ah3mod
  implicit none
  type(ah3_header_t) :: h
  type(ah3_grid_t) :: g
  integer :: i1, i2, ii

  character :: dateid*8, runid*1, indir*256, outprefix*15, prefix*10
  integer :: timeid

  print *, 'Date ID (e.g., 29112005)'
  read '(a)', dateid
  print *, 'Run ID (e.g., a)'
  read '(a)', runid
  print *, 'Time ID (e.g., 70)'
  read *, timeid

  print *, 'Size of central region?'
  read *, ii

  call ah3setinputdir('./')     !just use local directory

  call ah3readfile(dateid, runid, timeid, h, g)
 
  print *, 'maximum density is ', maxval(g%data(:,:,:,1))
  print *, 'at ', maxloc(g%data(:,:,:,1))

  ! assume grid size is same in all dimensions 
  i1 = g%n(1)/2 + 1 - ii
  i2 = i1 + 2*ii - 1

  print *, 'Mass removed is ',&
       &  0.9*sum(g%data(i1:i2, i1:i2, i1:i2, 1))*product(g%cellSizes) / &
       &  2.e33, ' solar masses'

  g%data(i1:i2, i1:i2, i1:i2, :) = 0.1*g%data(i1:i2, i1:i2, i1:i2, :)

  print *, 'New run ID (e.g., x)'
  read '(a)', runid

  call ah3writefile(dateid, runid, timeid, h, g)
 


end program ah3doctor
