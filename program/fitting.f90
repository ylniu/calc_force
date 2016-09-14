subroutine fitting(np, pow, nz, x, y, df, df_fit, info)
	use kinds, only: DP
	use input, only: if_fit, freq0, freqoffset, set
	implicit none
	!----------------------------------------------------------------------------
	integer , intent( in) :: nz
	integer , intent( in) :: np
	real(DP), intent( in) :: pow(np)
	real(DP), intent( in) :: x(nz), y(nz), df(nz)
	real(DP), intent(out) :: df_fit(nz)
	integer , intent(out) :: info
	!----------------------------------------------------------------------------
	integer               :: i, j, nsmooth, nderiv
	real(DP), allocatable :: df_omega  (:)
	real(DP), allocatable :: df_smooth1(:)
	real(DP), allocatable :: df_smooth2(:)
	real(DP), allocatable :: df_smooth (:)
	real(DP), allocatable :: df_deriv  (:)
	real(DP), allocatable :: y_smooth1 (:)
	real(DP), allocatable :: y_smooth2 (:)
	real(DP), allocatable :: y_deriv   (:)
	real(DP), allocatable :: int_z     (:)
	!----------------------------------------------------------------------------
	nsmooth = nz - 4 * set
	nderiv  = nsmooth - 2
	!----------------------------------------------------------------------------
	allocate(df_omega  (nz        ))
	allocate(df_smooth1(nz-2*set  ))
	allocate(df_smooth2(nsmooth   ))
	allocate(df_smooth (nderiv    ))
	allocate(df_deriv  (nderiv    ))
	allocate(y_smooth1 (nz-2*set  ))
	allocate(y_smooth2 (nsmooth   ))
	allocate(y_deriv   (nderiv    ))
	allocate(int_z     (nderiv    ))
	!----------------------------------------------------------------------------
	info     = 0
	!----------------------------------------------------------------------------
	if (if_fit) then
		call fit_polynomial(np, pow, nz, y, df, df_fit, info)
	else
		df_fit = df
	end if
	!----------------------------------------------------------------------------
	df_omega = (df_fit - freqoffset ) / freq0
	!----------------------------------------------------------------------------
	! set = 2
	! 1 2 3 4 5 6 7 8 9 10
	! 1 2 3 4 5
	!   2 3 4 5 6
	!     ......
	!           6 7 8 9 10
	!     3 4 5 6 7 8
	call smooth(nz      , set, df_omega  , df_smooth1, y, y_smooth1)
	call smooth(nz-2*set, set, df_smooth1, df_smooth2, y_smooth1, y_smooth2)
	call deriv (nsmooth, df_smooth2, df_smooth, df_deriv, y_smooth2, y_deriv)
	call get_int_z(nderiv, y_deriv, df_smooth, df_deriv, int_z)
	!----------------------------------------------------------------------------
	deallocate(df_omega  )
	deallocate(df_smooth1)
	deallocate(df_smooth2)
	deallocate(df_smooth )
	deallocate(df_deriv  )
	deallocate(y_smooth1 )
	deallocate(y_smooth2 )
	deallocate(y_deriv   )
	deallocate(int_z     )
	!----------------------------------------------------------------------------
	return
end subroutine
