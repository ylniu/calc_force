function number_of_line(fname, info)
	implicit none
	!----------------------------------------------------------------------------
	character(*), intent( in) :: fname
	integer     , intent(out) :: info
	integer                   :: number_of_line
	!----------------------------------------------------------------------------
	integer                   :: fid, ios, ios1
	!----------------------------------------------------------------------------
	fid = 88
	number_of_line=0
	open(fid, file=fname, status="old", iostat=ios)
		read(fid, *, iostat=ios1)
		do while (ios1==0)
			number_of_line = number_of_line + 1
			read(fid, *, iostat=ios1)
		end do
	close(fid)
	if (ios /=0) info=ios
	if (ios1/=0) info=ios
	!----------------------------------------------------------------------------
	return
	!----------------------------------------------------------------------------
end function