subroutine (nsmooth, df_smooth, df_deriv, y_smooth, y_deriv)
	use kinds, only: DP
	implicit none
	!----------------------------------------------------------------------------
	integer , intent( in) :: nsmooth
	real(DP), intent( in) :: df_smooth(nsmooth  )
	real(DP), intent( in) :: y_smooth (nsmooth  )
	real(DP), intent(out) :: df_deriv (nsmooth-2)
	real(DP), intent(out) :: y_deriv  (nsmooth-2)
	!----------------------------------------------------------------------------
	integer               :: i
	!----------------------------------------------------------------------------
	df_smooth = 0.0_DP
	do i=1, nsmooth-2
		y_deriv (i) = y_smooth (i+1)
		df_deriv(i) = (df_smooth(i+2) - df_smooth(i)) / (y_smooth(i+2) - y_smooth(i))
	end do
	!----------------------------------------------------------------------------
	return
end subroutine
