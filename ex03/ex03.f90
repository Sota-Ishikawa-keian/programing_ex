program ex03
    use, intrinsic :: iso_fortran_env
    use mod_linear_solver_lu

    implicit none

    real(real64), allocatable :: A(:, :), Ainv(:, :)

    integer :: i

    !使い分け
    integer :: n = 3
    !integer :: n = 2

    allocate (A(n, n))
    allocate (Ainv(n, n))

    !使い分け
    A = reshape([1.0d0, -2.0d0, 0.0d0, 1.0d0, 0.0d0, 2.0d0, -1.0d0, 1.0d0, 1.0d0], [n, n])
    !A = reshape([0.0d0, 2.0d0, 1.0d0, 1.0d0, 3.0d0, 0.0d0, 0.0d0, 1.0d0, 2.0d0], [n, n])
    !A = reshape([0.0d0, 2.0d0, 1.0d0, 3.0d0], [n, n])

    print *, "A ="
    do i = 1, n
        write(*, '(*(F25.15,1X))') A(i, :)
    end do

    call matrix_inverse(A, Ainv)


    print *, "Ainv ="
    do i = 1, n
        write(*, '(*(F25.15,1X))') Ainv(i, :)
    end do

    deallocate (A, Ainv)


end program ex03
