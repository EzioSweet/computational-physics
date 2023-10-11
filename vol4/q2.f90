program homework
  implicit none
  integer:: i,j,k
  real*8,dimension(16)::x,y,a
  real*8,external::phi
  real*8::result,tmp
  do i=1,16
     x(i)=-5d0+10d0/15.0*(i-1)
     y(i)=1d0/(1d0+x(i)**2)
  end do
  do i=1,16
      if(i==1) then
          a(1)=y(1)
      else
          tmp=y(i)
          do j =1,i-1
              tmp =( tmp-a (j) )/(x(i) - x(  j  ))
          end do
          a(i)=tmp
      end if
  end do
  open(50,file="result.txt")
  do i=-500,500
      result = a(1)
      do j=2,16
          tmp=1
          do k=1,j-1
              tmp=tmp*(i/100d0-x(k))
          end do
          result = result+a(j)*tmp
      end do
      write(50,"(f10.4,a2,f10.4)") i/100d0,",",result
  end do
end program homework
