subroutine smooth(nz, set, df_omega, df_smooth, y, y_smooth)
	use kinds, only: DP
	implicit none
	!----------------------------------------------------------------------------
	integer , intent( in) :: nz
	integer , intent( in) :: set
	real(DP), intent( in) :: df_omega (nz      )
	real(DP), intent( in) :: y        (nz      )
	real(DP), intent(out) :: df_smooth(nz-2*set)
	real(DP), intent(out) :: y_smooth (nz-2*set)
	!----------------------------------------------------------------------------
	integer               :: i, j
	!----------------------------------------------------------------------------
	df_smooth = 0.0_DP
	do i=1, nz-2*set
		y_smooth(i) = y(i+set)
		do j=1, 5
			df_smooth(i) = df_smooth(i) + df_omega(i+j-1)
		end do
	end do
	!----------------------------------------------------------------------------
	return
end subroutine
