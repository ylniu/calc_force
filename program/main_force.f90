program main_force
	use kinds, only: DP
	use funs , only: gaussj
	implicit none
	!----------------------------------------------------------------------------
	integer               :: i, j, nz, nr, iz, ir
	integer               :: fid, info
	real(DP)              :: nm, kspring, amp, f0, set
	real(DP)              :: freqoffset, forceconvert, energyconvert
	integer               :: np
	integer               :: power_min
	integer               :: power_max
	real(DP)              :: imgscale
	real(DP), allocatable :: x (:,:)
	real(DP), allocatable :: y (:,:)
	real(DP), allocatable :: df(:,:)
	real(DP), allocatable :: df_fit(:,:)
	real(DP), allocatable :: pow(:)
	real(DP)              :: a(3,3), b(3,1)
	logical               :: if_fit, if_debug, if_debug_gaussj
	character(200)        :: finp, fdata, fout
	!----------------------------------------------------------------------------
	namelist /control/ if_fit, imgscale, fdata, fout, power_min, power_max
	!----------------------------------------------------------------------------
	!input data-frequecy shift
	!
	fid             = 1
	if_fit          = .false.
	if_debug        = .true.
	if_debug_gaussj = .false.
	power_min       = -8
	power_max       =  4
	imgscale        = 3.0
	!set directory
	!--------------------------------------sub-----------------------------------
	call getarg(1, finp)
	!----------------------------------------------------------------------------
	open(fid, file=finp, status="old")
		read(fid, control)
	close(fid)
	np = power_max - power_min + 1
	!----------------------------------------------------------------------------
	call get_data_number(fdata,nz,nr,info)
	!----------------------------------------------------------------------------
	allocate(x     (nz,nr))
	allocate(y     (nz,nr))
	allocate(df    (nz,nr))
	allocate(df_fit(nz,nr))
	allocate(pow   (np   ))
	!----------------------------------------------------------------------------
	do i=1, np
		pow(i) = real(power_min + i - 1, DP)
	end do
	!----------------------------------------------------------------------------
	call get_data_matrix(fdata,nz,nr,x,y,df,info)
	!----------------------------------------------------------------------------
	!
	do iz = 1, nz
		!write(*,'(3(2x, es20.10))') x(iz, 2), y(iz, 2), df(iz, 2)
	end do
	!----------------------------------------------------------------------------
	! Test Gauss-Jordan Elimination
	!
	if (if_debug_gaussj) then
		data a /1.0, 4.0, 7.0, 2.0, 8.0, 10.0, 3.0, 6.0, 8.0/
		data b /1.0, 2.0, 2.0/
		call gaussj(a,b,if_debug)
	end if
	!----------------------------------------------------------------------------
	!
	do ir=1, nr
		call fitting(np, pow, nz, x(1,ir), y(1,ir), df(1,ir), df_fit(1,ir), if_fit, info)
	end do
	open(fid, file=fout)
		do ir=1, nr
			do iz=1, nz
				write(fid, '(3es24.16)') x(iz,ir), y(iz,ir), df_fit(iz,ir)
			end do
		end do
	close(fid)
	write(*,*)
	write(*,'(2x, "Input  file", 2x, a)') trim(fdata)
	write(*,'(2x, "Output file", 2x, a)') trim(fout)
	write(*,'(2x, "Normal terminated!")')
	!----------------------------------------------------------------------------
	deallocate(x     )
	deallocate(y     )
	deallocate(df    )
	deallocate(df_fit)
	deallocate(pow   )
	!----------------------------------------------------------------------------
	stop
end
