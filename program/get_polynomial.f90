subroutine get_polynomial(npower, powers, nz, x, y, C, D, info)
	use kinds, only: DP
	implicit none
	!----------------------------------------------------------------------------
	integer , intent( in) :: npower, nz
	integer , intent( in) :: powers(npower)
	real(DP), intent( in) :: x(nz), y(nz), df(nz)
	integer , intent(out) :: info
	real(DP), intent(out) :: C(npower, npower)
	real(DP), intent(out) :: D(npower)
	!----------------------------------------------------------------------------
	integer               :: i, j, r
	real(DP), allocatable :: g(:,:)
	real(DP), allocatable :: C(:,:)
	real(DP), allocatable :: D(:)
	!----------------------------------------------------------------------------
	info = 0
	!----------------------------------------------------------------------------
	! Calculate g(r,i)
	!
	do r=1, nz
		do i=1, npower
			g(r,i) = x(r)**powers(i)
		end do
	end do
	!----------------------------------------------------------------------------
	! Calculate Cji
	!
	C = 0.D0
	do i=1, npower
		do j=1, npower
			do r=1, nz
				C(j,i) = C(j,i) + 2.0_DP * g(r,i) * g(r,j)
		end do
	end do
	!----------------------------------------------------------------------------
	! Calculate Dj
	!
	D = 0.D0 
	do j=1, npower
		do r=1, nz
			D(j) = D(j) + 2.0_DP * y(r) * g(r,j)
	end do
	!----------------------------------------------------------------------------
	return
end subroutine
