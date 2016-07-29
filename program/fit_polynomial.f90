subroutine fit_polynomial(np, pow, nz, x, y, y_fit, info)
	use kinds, only: DP
	use funs , only: gaussj
	implicit none
	!----------------------------------------------------------------------------
	integer , intent( in) :: np, nz
	real(DP), intent( in) :: pow(np)
	real(DP), intent( in) :: x(nz), y(nz)
	real(DP), intent(out) :: y_fit(nz)
	integer , intent(out) :: info
	!----------------------------------------------------------------------------
	integer               :: i, j, iz, ip, jp
	real(DP), allocatable :: g(:,:)
	real(DP), allocatable :: C(:,:)
	real(DP), allocatable :: D(:,:)
	real(DP), allocatable :: d_local(  :)
	real(DP), allocatable :: y_local(:,:)
	logical               :: if_debug
	logical               :: if_debug_d_local
	!----------------------------------------------------------------------------
	allocate(g(nz,np))
	allocate(C(np,np))
	allocate(D(np, 1))
	!----------------------------------------------------------------------------
	info             = 0
	C                = 0.0_DP
	D                = 0.0_DP
	y_fit            = 0.0_DP
	if_debug         = .false.
	if_debug_d_local = .false.
	!----------------------------------------------------------------------------
	! Calculate g(iz,ip)
	!
	do iz=1, nz
		do ip=1, np
			g(iz,ip) = x(iz)**pow(ip)
		end do
	end do
	!----------------------------------------------------------------------------
	! Calculate Cji
	!
	do ip=1, np
		do jp=1, np
			do iz=1, nz
				C(jp,ip) = C(jp,ip) + 2.0_DP * g(iz,ip) * g(iz,jp)
			end do
		end do
	end do
	!----------------------------------------------------------------------------
	! Calculate Dj
	!
	do jp=1, np
		do iz=1, nz
			D(jp,1) = D(jp,1) + 2.0_DP * y(iz) * g(iz,jp)
		end do
	end do
	!----------------------------------------------------------------------------
	call gaussj(C,D,if_debug)
	!----------------------------------------------------------------------------
	! Calculate y_fit
	!
	do iz=1, nz
		do jp=1, np
			y_fit(iz) = y_fit(iz) + D(jp,1) * g(iz,jp)
		end do
	end do
	!
	!----------------------------------------------------------------------------
	if (if_debug_d_local) then
		allocate(d_local(   np))
		allocate(y_local(nz,np))
		y_local = 0.0_DP
		d_local = 0.0_DP
		!-------------------------------------------------------------------------
		! Calculate d_local
		!
		do iz=1, nz
			do jp=1, np
				y_local(iz,jp) = y_local(iz,jp) + D(jp,1) * g(iz,jp)
				d_local(   jp) = d_local(   jp) + (y_local(iz,jp) - y(iz))**2.D0
			end do
		end do
		d_local = sqrt(d_local)
		d_local = d_local / nz
		!-------------------------------------------------------------------------
		do jp=1, np
			write(*,'(f10.2, f15.6)') pow(jp), d_local(jp)
		end do
		deallocate(d_local)
		deallocate(y_local)
	end if
	!----------------------------------------------------------------------------
	deallocate(g    )
	deallocate(C    )
	deallocate(D    )
	!----------------------------------------------------------------------------
	return
end subroutine
