module kinds
	implicit none
	integer, parameter :: DP=8
end module

module funs
	implicit none
	Interface
		Subroutine gaussj(a,b)
			Real a(:,:)
			Real b(:,:)
		End Subroutine gaussj
	End Interface
end module
