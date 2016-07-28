subroutine get_data_matrix(fdata,nz,nr,x,y,df,info)
	use kinds, only: DP
	implicit none
	!----------------------------------------------------------------------------
	character(*), intent( in) :: fdata
	integer     , intent( in) :: nz, nr
	integer     , intent(out) :: info
	real(DP)    , intent(out) :: x (nz, nr)
	real(DP)    , intent(out) :: y (nz, nr)
	real(DP)    , intent(out) :: df(nz, nr)
	!----------------------------------------------------------------------------
	integer                   :: ir, iz, ios
	integer                   :: fid
	character(200)            :: line
	!----------------------------------------------------------------------------
	fid  = 77
	info = 0
	open(fid, file=fdata, status="old", iostat=ios)
		do ir=1, nr
			do iz=1, nz
				read(fid, '(a)') line
				read(line, *) x(iz, ir), y(iz, ir), df(iz, ir)
			end do
		end do
	close(fid)
	if (ios/=0) info = ios
	!----------------------------------------------------------------------------
	return
end subroutine