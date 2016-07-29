module kinds
	implicit none
	integer, parameter :: DP=8
end module

module funs
	implicit none
	Interface
		Subroutine gaussj(a,b,d)
			use kinds, only: DP
			Real(DP) :: a(:,:)
			Real(DP) :: b(:,:)
			logical  :: d
		End Subroutine gaussj
	End Interface
end module

module input
	use kinds, only: DP
	implicit none
	!----------------------------------------------------------------------------
	logical        :: if_fit
	integer        :: set
	real(DP)       :: kspring
	real(DP)       :: amp
	real(DP)       :: freq0
	real(DP)       :: power_min
	real(DP)       :: power_max
	character(200) :: fdata
	character(200) :: fout
	! Frequency offset at large distances. Ensures frequency shift
	! in given data is zero at large separations. (Hz):
	real(DP)       :: freqoffset
	!----------------------------------------------------------------------------
end module

module const
	use kinds, only: DP
	implicit none
	!----------------------------------------------------------------------------
	real(DP), parameter :: nm = 1.0E0_DP ! Nanometre definition
	!----------------------------------------------------------------------------
end module
