subroutine fitting(np, pow, nz, x, y, df, df_fit, if_fit, info)
	use kinds, only: DP
	implicit none
	!----------------------------------------------------------------------------
	integer , intent( in) :: nz
	integer , intent( in) :: np
	real(DP), intent( in) :: pow(np)
	real(DP), intent( in) :: x(nz), y(nz), df(nz)
	logical , intent( in) :: if_fit
	real(DP), intent(out) :: df_fit(nz)
	integer , intent(out) :: info
	!----------------------------------------------------------------------------
	integer               :: i, j
	!----------------------------------------------------------------------------
	info     = 0
	!----------------------------------------------------------------------------
	if (if_fit) then
		call fit_polynomial(np, pow, nz, y, df, df_fit, info)
	else
		df_fit = df
	end if
	!----------------------------------------------------------------------------
	return
end subroutine
