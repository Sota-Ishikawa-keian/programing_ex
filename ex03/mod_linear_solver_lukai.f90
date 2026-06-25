module mod_linear_solver_lukai

  use, intrinsic :: iso_fortran_env
  implicit none
  private
  public :: inverse_matrix_lu

contains

!====================================================
! 逆行列計算
!====================================================
  subroutine inverse_matrix_lu(mat, matinv)

    real(real64), intent(in)  :: mat(:,:)
    real(real64), intent(out) :: matinv(:,:)

    integer :: n

    real(real64), allocatable :: P(:,:), L(:,:), U(:,:)
    real(real64), allocatable :: Linv(:,:), Uinv(:,:)

    n = size(mat,1)

    if(size(mat,2) /= n) then
      print *, "Error : matrix must be square."
      stop
    end if

    allocate(P(n,n),L(n,n),U(n,n))
    allocate(Linv(n,n),Uinv(n,n))

    call LU_decomposition(mat,L,U,P)

    call invert_L(L,Linv)

    call invert_U(U,Uinv)

    matinv = matmul(Uinv,matmul(Linv,P))

    deallocate(P,L,U,Linv,Uinv)

  end subroutine inverse_matrix_lu

!====================================================
! LU分解
! PA = LU
!====================================================
  subroutine LU_decomposition(A,L,U,P)

    real(real64), intent(in)  :: A(:,:)
    real(real64), intent(out) :: L(:,:), U(:,:), P(:,:)

    integer :: n, i, k

    n = size(A,1)

    U = A

    L = 0.0_real64
    P = 0.0_real64

    do i = 1,n
      L(i,i) = 1.0_real64
      P(i,i) = 1.0_real64
    end do

    do k = 1,n-1

      call Pivot(U,L,P,k)

      if(abs(U(k,k)) < 1.0d-14) then
        print *, "Singular matrix."
        stop
      end if

      call Eliminate(U,L,k)

    end do

  end subroutine LU_decomposition

!====================================================
! ピボット選択
!====================================================
  subroutine Pivot(U,L,P,k)

    real(real64), intent(inout) :: U(:,:), L(:,:), P(:,:)

    integer, intent(in) :: k

    integer :: n
    integer :: i
    integer :: pivot_row

    real(real64) :: max_value

    real(real64) :: tempU(size(U,2))
    real(real64) :: tempP(size(P,2))
    real(real64) :: tempL(size(L,2))

    n = size(U,1)

    pivot_row = k
    max_value = abs(U(k,k))

    do i = k+1,n

      if(abs(U(i,k)) > max_value) then

        max_value = abs(U(i,k))
        pivot_row = i

      end if

    end do

    if(pivot_row /= k) then

      tempU = U(k,:)
      U(k,:) = U(pivot_row,:)
      U(pivot_row,:) = tempU

      tempP = P(k,:)
      P(k,:) = P(pivot_row,:)
      P(pivot_row,:) = tempP

      if(k > 1) then

        tempL(1:k-1) = L(k,1:k-1)

        L(k,1:k-1) = L(pivot_row,1:k-1)

        L(pivot_row,1:k-1) = tempL(1:k-1)

      end if

    end if

  end subroutine Pivot

!====================================================
! ガウス消去
!====================================================
  subroutine Eliminate(U,L,k)

    real(real64), intent(inout) :: U(:,:), L(:,:)
    integer, intent(in) :: k

    integer :: n, i, j
    real(real64) :: factor

    n = size(U,1)

    do i = k+1,n

      factor = U(i,k)/U(k,k)

      L(i,k) = factor

      do j = k,n

        U(i,j) = U(i,j) - factor*U(k,j)

      end do

    end do

  end subroutine Eliminate

!====================================================
! L^{-1}
!====================================================
  subroutine invert_L(L,Linv)

    real(real64), intent(in)  :: L(:,:)
    real(real64), intent(out) :: Linv(:,:)

    integer :: n
    integer :: i
    integer :: j
    integer :: k

    real(real64) :: s

    n = size(L,1)

    Linv = 0.0_real64

    do j = 1,n

      Linv(j,j) = 1.0_real64/L(j,j)

      do i = j+1,n

        s = 0.0_real64

        do k = j,i-1

          s = s + L(i,k)*Linv(k,j)

        end do

        Linv(i,j) = -s/L(i,i)

      end do

    end do

  end subroutine invert_L

!====================================================
! U^{-1}
!====================================================
  subroutine invert_U(U,Uinv)

    real(real64), intent(in)  :: U(:,:)
    real(real64), intent(out) :: Uinv(:,:)

    integer :: n
    integer :: i
    integer :: j
    integer :: k

    real(real64) :: s

    n = size(U,1)

    Uinv = 0.0_real64

    do j = n,1,-1

      Uinv(j,j) = 1.0_real64/U(j,j)

      do i = j-1,1,-1

        s = 0.0_real64

        do k = i+1,j

          s = s + U(i,k)*Uinv(k,j)

        end do

        Uinv(i,j) = -s/U(i,i)

      end do

    end do

  end subroutine invert_U

end module mod_linear_solver_lukai
