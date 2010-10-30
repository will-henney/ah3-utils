program ah3test
  use ah3mod
  implicit none
  type(ah3_header_t) :: h
  type(ah3_grid_t) :: g

  character :: dateid*8, runid*1, indir*256, outprefix*15, prefix*10
  integer :: timeid


  print *, 'Date ID (e.g., 29112005)'
  read '(a)', dateid
  print *, 'Run ID (e.g., a)'
  read '(a)', runid
  print *, 'Time ID (e.g., 70)'
  read *, timeid

  call ah3setinputdir('./')
  call ah3readfile(dateid, runid, timeid, h, g)
  call ah3writefile(dateid, 'Z', timeid, h, g)

end program ah3test
