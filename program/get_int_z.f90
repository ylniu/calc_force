subroutine get_int_z(nderiv, y_deriv, df_smooth, df_deriv, inttable)
	use kinds, only: DP
	use input, only: amp
	implicit none
	!----------------------------------------------------------------------------
	integer , intent( in) :: nderiv
	real(DP), intent( in) :: df_smooth(nderiv)
	real(DP), intent( in) :: df_deriv (nderiv)
	real(DP), intent( in) :: y_deriv  (nderiv)
	real(DP), intent(out) :: inttable    (nderiv)
	!----------------------------------------------------------------------------
	integer               :: iz, it
	real(DP)              :: PI
	real(DP)              :: z, dt, t
	real(DP), allocatable :: correct(:,:)
	!----------------------------------------------------------------------------
	allocate(correct(nderiv,0:2))
	!----------------------------------------------------------------------------
	PI       = acos(-1.0_DP)
	inttable = 0.0_DP
	correct  = 0.0_DP
	do iz=1, nderiv
		z = y_deriv(iz)
		!-------------------------------------------------------------------------
		if (iz<nderiv) then
			z1 = y_deriv(iz+1)
			correct(0,iz) = df_smooth(iz) * (z1 - z)
			correct(1,iz) =  2.0_DP * (sqrt(amp) / 8.0_DP / sqrt(pi)) * df_smooth(iz) * sqrt(z1-z)
			correct(2,iz) = -2.0_DP * (sqrt(amp)**3 / sqrt(2.0_DP)) df_deriv(iz) * sqrt(z1-z)
		end if
		!-------------------------------------------------------------------------
		do it=iz+1, nderiv
			t  = y_deriv(it)
			dt = y_deriv(it) - y_deriv(it-1)
			!----------------------------------------------------------------------
			inttable(iz) = inttable(iz) &
				+ (1.0_DP + sqrt(amp) / 8.0_DP / sqrt(PI * (t-z))) * df_smooth(it) &
				- sqrt(amp)**3 / sqrt(2*(t-z)) * df_deriv(it)
			!----------------------------------------------------------------------
		end do
	end do
	!----------------------------------------------------------------------------
	deallocate(correct)
	!----------------------------------------------------------------------------
	return
end subroutine
