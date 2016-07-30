subroutine get_int_z(nderiv, y_deriv, df_smooth, df_deriv, int_z)
	use kinds, only: DP
	use input, only: amp
	implicit none
	!----------------------------------------------------------------------------
	integer , intent( in) :: nderiv
	real(DP), intent( in) :: df_smooth(nderiv)
	real(DP), intent( in) :: df_deriv (nderiv)
	real(DP), intent( in) :: y_deriv  (nderiv)
	real(DP), intent(out) :: int_z    (nderiv)
	!----------------------------------------------------------------------------
	integer               :: iz, it
	real(DP)              :: PI
	real(DP)              :: z, dt, t
	real(DP), allocatable :: correct(:,:)
	!----------------------------------------------------------------------------
	allocate(correct(3,nderiv))
	!----------------------------------------------------------------------------
	PI = acos(-1.0_DP)
	int_z = 0.0_DP
	do iz=1, nderiv
		z = y_deriv(iz)
		!correct(1,iz)
		do it=iz+1, nderiv
			t  = y_deriv(it)
			dt = y_deriv(it) - y_deriv(it-1)
			int_z(iz) = int_z(iz) + (1.0_DP + amp**0.5_DP / 8.0_DP / sqrt(PI * (t-z))) * df_smooth(it) &
				- amp**1.5_DP / sqrt(2*(t-z)) * df_deriv(it)
		end do
	end do
	!----------------------------------------------------------------------------
	deallocate(correct)
	!----------------------------------------------------------------------------
	return
end subroutine
