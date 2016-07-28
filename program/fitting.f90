subroutine fitting(nz, x, y, df, info)
	use kinds, only: DP
	implicit none
	!----------------------------------------------------------------------------
	integer , intent( in) :: nz
	real(DP), intent( in) :: x(nz), y(nz), df(nz)
	integer , intent(out) :: info
	!----------------------------------------------------------------------------
	integer               :: i
	integer               :: npower
	integer               :: ipoly_min
	integer               :: ipoly_max
	integer, allocatable  :: powers(:)
	integer, allocatable  :: C(:,:)
	integer, allocatable  :: D(:)
	!----------------------------------------------------------------------------
	info      = 0
	ipoly_min = -4
	ipoly_max = 10
	npower    = ipoly_max - ipoly_min + 1
	allocate(powers(npower))
	allocate(C(npower,npower))
	allocate(D(npower))
	!----------------------------------------------------------------------------
	do i=1, npower
		powers(i) = ipoly_min + i - 1
	end do
	!----------------------------------------------------------------------------
	!call get_polynomial(npower, powers, nz, x, y, C, D, info)
	!----------------------------------------------------------------------------
	deallocate(powers)
	deallocate(C)
	deallocate(D)
	!----------------------------------------------------------------------------
	return
end subroutine
