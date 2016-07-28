subroutine get_data_number(fdata, nz, nr, info)
	use kinds, only: DP
	implicit none
	!----------------------------------------------------------------------------
	character(*), intent( in) :: fdata
	integer     , intent(out) :: info
	integer     , intent(out) :: nz, nr
	!----------------------------------------------------------------------------
	integer                   :: nline
	integer                   :: fid
	!----------------------------------------------------------------------------
	real(DP)                  :: x_old, x_new
	!----------------------------------------------------------------------------
	integer     , external    :: number_of_line
	!----------------------------------------------------------------------------
	nline = number_of_line(fdata, info)
	!----------------------------------------------------------------------------
	fid = 77
	open(fid, file=fdata, status="old")
		read(fid, *) x_new
		x_old = x_new
		nz    = 0
		do while (x_old==x_new)
			nz = nz + 1
			x_old = x_new
			read(fid, *) x_new
		end do
	close(fid)
	!----------------------------------------------------------------------------
	nr = nline / nz
	!----------------------------------------------------------------------------
	return
end subroutine