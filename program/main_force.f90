program main_force
	use kinds, only: DP
	implicit none
	!----------------------------------------------------------------------------
	integer               :: i, j, nz, nr, iz, ir
	integer               :: fid, info
	real(DP)              :: nm, kspring, amp, f0, set
	real(DP)              :: freqoffset, forceconvert, energyconvert
	real(DP)              :: imgscale
	real(DP), allocatable :: x (:,:)
	real(DP), allocatable :: y (:,:)
	real(DP), allocatable :: df(:,:)
	logical               :: if_fit
	character(200)        :: finp, fdata, fout
	!----------------------------------------------------------------------------
	namelist /control/ if_fit, imgscale, fdata, fout
	!----------------------------------------------------------------------------
	!input data-frequecy shift
	!
	fid      = 1
	if_fit   = .false.
	imgscale = 3.0
	!set directory
	!--------------------------------------sub-----------------------------------
	call getarg(1, finp)
	!----------------------------------------------------------------------------
	open(fid, file=finp, status="old")
		read(fid, control)
	close(fid)
	!----------------------------------------------------------------------------
	call get_data_number(fdata,nz,nr,info)
	!----------------------------------------------------------------------------
	allocate(x (nz,nr))
	allocate(y (nz,nr))
	allocate(df(nz,nr))
	!----------------------------------------------------------------------------
	call get_data_matrix(fdata,nz,nr,x,y,df,info)
	!----------------------------------------------------------------------------
	!
	do iz = 1, nz
		!write(*,'(3(2x, es20.10))') x(iz, 2), y(iz, 2), df(iz, 2)
	end do
	!----------------------------------------------------------------------------
	do ir=1, nr
		call fitting(nz, nr, ir, x, y, df, info)
	end do
	!----------------------------------------------------------------------------
	deallocate(x )
	deallocate(y )
	deallocate(df)
	!----------------------------------------------------------------------------
	stop
end
