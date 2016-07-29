	SUBROUTINE gaussj(a,b,if_debug)
	USE nrtype; USE nrutil, ONLY : assert_eq,nrerror,outerand,outerprod,swap
	IMPLICIT NONE
	REAL(DP), DIMENSION(:,:), INTENT(INOUT) :: a,b
	logical :: if_debug
	INTEGER(I4B), DIMENSION(size(a,1)) :: ipiv,indxr,indxc
	LOGICAL(LGT), DIMENSION(size(a,1)) :: lpiv
	REAL(DP) :: pivinv
	REAL(DP), DIMENSION(size(a,1)) :: dumc
	INTEGER(I4B), TARGET :: irc(2)
	INTEGER(I4B) :: i,l,n, i1, i2, na1, na2, nb1, nb2
	character(200) :: fmt
	INTEGER(I4B), POINTER :: irow,icol
	na1=size(a,1)
	na2=size(a,2)
	nb1=size(b,1)
	nb2=size(b,2)
	n=assert_eq(na1,na2,nb1,'gaussj')
	irow => irc(1)
	icol => irc(2)
	ipiv=0
	if (if_debug) then
		write(fmt,'("(",i0,"es12.3",",2x,",i0,"es12.3)")') na2, nb2
		write(*,*) "Begin"
		do i1 = 1, na1
			write(*,fmt) (a(i1, i2), i2=1, na2), (b(i1, i2), i2=1, nb2)
		end do
		write(*,*)
	end if
	do i=1,n
		lpiv = (ipiv == 0)
		irc=maxloc(abs(a),outerand(lpiv,lpiv))
		ipiv(icol)=ipiv(icol)+1
		if (ipiv(icol) > 1) call nrerror('gaussj: singular matrix (1)')
		if (irow /= icol) then
			call swap(a(irow,:),a(icol,:))
			call swap(b(irow,:),b(icol,:))
		end if
		indxr(i)=irow
		indxc(i)=icol
		if (a(icol,icol) == 0.0) &
			call nrerror('gaussj: singular matrix (2)')
		pivinv=1.0_sp/a(icol,icol)
		! commented by niuyingli
		! a(icol,icol)=1.0
		a(icol,:)=a(icol,:)*pivinv
		b(icol,:)=b(icol,:)*pivinv
		dumc=a(:,icol)
		! commented by niuyingli
		! a(:,icol)=0.0
		! a(icol,icol)=pivinv
		a(1:icol-1,:)=a(1:icol-1,:)-outerprod(dumc(1:icol-1),a(icol,:))
		b(1:icol-1,:)=b(1:icol-1,:)-outerprod(dumc(1:icol-1),b(icol,:))
		a(icol+1:,:)=a(icol+1:,:)-outerprod(dumc(icol+1:),a(icol,:))
		b(icol+1:,:)=b(icol+1:,:)-outerprod(dumc(icol+1:),b(icol,:))
		if (if_debug) then
			write(*,*) "Cycle", i
			do i1 = 1, na1
				write(*,fmt) (a(i1, i2), i2=1, na2), (b(i1, i2), i2=1, nb2)
			end do
			write(*,*)
		end if
	end do
	! commented by niuyingli
	!do l=n,1,-1
	!	call swap(a(:,indxr(l)),a(:,indxc(l)))
	!end do
	if (if_debug) then
		write(*,*) "Results"
		do i1 = 1, na1
				write(*,fmt) (a(i1, i2), i2=1, na2), (b(i1, i2), i2=1, nb2)
			end do
		write(*,*)
	end if
	END SUBROUTINE gaussj
