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
