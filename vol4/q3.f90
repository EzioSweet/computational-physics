subroutine solve(A,x,b)
    integer :: i, j, k, r
    real*8, dimension(14, 14) :: A, u, l
    real*8, dimension(14) :: x, b, y
    real*8 :: sum
    u = 0.0d0
    l = 0.0d0
    u(1, :) = A(1, :)
    l(:, 1) = A(:, 1) / u(1, 1)
    do k = 2, 14
        do j = k, 14
            sum = 0
            do r = 1, k - 1
                sum = sum + l(k, r) * u(r, j)
            end do
            u(k, j) = A(k, j) - sum
        end do
        do i = k, 14
            if (i == k) then
                l(i, i) = 1
            else
                sum = 0
                do r = 1, k - 1
                    sum = sum + l(i, r) * u(r, k)
                end do
                l(i, k) = (A(i, k) - sum) / u(k, k)
            end if
        end do
    end do
    y(1) = b(1) / l(1, 1)
    do i = 2, 14
        sum = 0
        do j = 1, i - 1
            sum = sum + l(i, j) * y(j)
        end do
        y(i) = (b(i) - sum) / l(i, i)
    end do
    x(14) = y(14) / u(14, 14)
    do i = 13, 1, -1
        sum = 0
        do j = 14, i + 1, -1
            sum = sum + u(i, j) * x(j)
        end do
        x(i) = (y(i) - sum) / u(i, i)
    end do
end subroutine solve

program homework
  implicit none
  integer:: i,j
  real*8,dimension(16)::x,y,mm
  real*8,dimension(14,14)::A
  real*8,dimension(14)::b,m,s,u
  real*8,dimension(15)::h
  real*8,external::phi
  real*8::result,k,tmp
  do i=1,16
     x(i)=-5d0+10d0/15.0*(i-1)
     y(i)=1d0/(1d0+x(i)**2)
  end do
  do i=1,15
      h(i)=x(i+1)-x(i)
  end do
  A = 0d0
  do i=1,14
      A(i,i)=2
      u(i)=h(i)/(h(i)+h(i+1))
      s(i)=1-u(i)
      b(i)=3*(h(i+1)*(y(i+1)-y(i))/(h(i+1)+h(i))/h(i)&
              +h(i)*(y(i+2)-y(i+1))/(h(i+1)+h(i))/h(i+1))
  end do
  do i=1,13
      A(i,i+1)=u(i)
      A(i+1,i)=s(i+1)
  end do
  call solve(A,m,b)
  mm=0d0
  mm(2:15)=m(:)
  print *,y
  open(50,file="result.txt")
  do i=-500,500
      k=i/100d0
      result=0d0
      do j=1,15
          if(k>x(j) .and. k<=x(j+1)) then
              result=(k-x(j))/h(j)
              tmp=result
              result=(1+2*result)*(result-1)*(result-1)*y(j)&
                      +(3-2*result)*result*result*y(j+1)&
                      +result*(result-1)*(result-1)*mm(j)&
                      +result*result*(result-1)*mm(j+1)
          end if
      end do
      write(50,"(f10.4,a2,f10.8)") k,",",result
  end do
end program homework
