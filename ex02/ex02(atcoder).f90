program ex02
    implicit none

    integer :: N
    integer :: push = 0
    integer :: light = 1
    integer :: i
    integer, allocatable :: a(:)
    integer, allocatable :: done(:)

    read *, N

    allocate(a(N), done(N))

    do i = 1, N
        read *, a(i)
    end do

    done = 0

    do
        if (done(light) == 1) then
            push = -1
            exit
        end if

        done(light) = 1
        light = a(light)
        push = push + 1

        if (light == 2) exit
    end do

    print *, push

end program ex02
