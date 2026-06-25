module mod_linear_solver_lu

    use ,intrinsic :: iso_fortran_env
    implicit none
    private
    public :: matrix_inverse

contains

!------------------------------------------------
!逆行列計算
!------------------------------------------------
    subroutine matrix_inverse(A, Ainv)

        real(real64), intent(in) :: A(:, :)
        real(real64), intent(out) :: Ainv(:, :)

        real(real64), allocatable :: P(:, :), L(:, :), U(:, :), Linv(:, :), Uinv(:, :)
        integer :: n, m

        n = size(A, 1)
        m = size(A, 2)

        !それぞれの行列にサイズを決定
        allocate(P(n, m), L(n, m), U(n, m), Linv(n, m), Uinv(n, m))

        call LUdecomposition(P, A, L, U)
        call Linverse(L, Linv)
        call Uinverse(U, Uinv)

        !Ainv = Uinv*Linv*Pだから
        Ainv = matmul(Uinv, matmul(Linv, P))

        !数値計算ではallocateしたらdeallocateするのが習慣らしい
        deallocate(P, L, U, Linv, Uinv)

    end subroutine


!------------------------------------------------
!LU分解
!------------------------------------------------

    subroutine LUdecomposition(P, A, L, U)

        real(real64), intent(in) :: A(:, :)
        real(real64), intent(out) :: P(:, :), L(:, :), U(:, :)

        !nは行列のサイズ，iはPとLの初期値として１を導入する，kはピボット操作の回数
        integer :: n, i, k

        n = size(A, 1)

        !Aは固定し，Uを動かして最終的にPA＝LUを目指す
        U = A

        !PとLは最初に全成分を0にしてからiを用いて対角成分に１を挿入
        P = 0.0d0
        L = 0.0d0
        do i = 1, n
            P(i, i) = 1.0d0
            L(i, i) = 1.0d0
        end do

        do k = 1, n-1

            call Pivot(P, L, U, k)
            call GaussElimination(L, U, k)

        end do

    end subroutine LUdecomposition


!-----------------------------------------------
!ピボット選択
!-----------------------------------------------

    subroutine Pivot(P, L, U, k)

        real(real64), intent(inout) :: P(:, :), L(:, :), U(:, :)
        integer, intent(in) :: k

        !nは行列Uのサイズ，iはピボットとなる最大絶対値を探索するループで使う，pivot_rowは最終的にk行と入れ替える行
        integer :: n, i, pivot_row

        !maxは探索する中での最大値，〇zは入れ替えを行う際基準となるk行を１行分丸ごと保存
        real(real64) :: max, Pz(size(P, 2)), Lz(size(L, 2)), Uz(size(U, 2))

        n = size(U, 1)
        pivot_row = k
        max = abs(U(k, k))

        do i = k+1, n

            if (abs(U(i, k)) > max) then
                max = abs(U(i, k))
                pivot_row = i
            end if

        end do

        !Uのピボットを見つけたら合わせてP，L，Uの成分を入れ替える
        !ただし，Lに関しては確定した対角成分以外を入れ替えるため発動する条件を付与する

        if (pivot_row /= k) then

            Pz = P(pivot_row, :)
            P(pivot_row, :) = P(k, :)
            P(k, :) = Pz

            Uz = U(pivot_row, :)
            U(pivot_row, :) = U(k, :)
            U(k, :) = Uz

        end if

        if (k > 1) then

            Lz(1:k-1) = L(pivot_row, 1:k-1)
            L(pivot_row, 1:k-1) = L(k, 1:k-1)
            L(k, 1:k-1) = Lz(1:k-1)

        end if

    end subroutine Pivot


!-----------------------------------------------
!ガウス消去
!-----------------------------------------------
    subroutine GaussElimination(L, U, k)

        real(real64), intent(inout) :: L(:, :), U(:, :)
        integer, intent(in) :: k

        !nは行列のサイズ，行列Uの成分をU(i,j)とし，それぞれの成分で計算を行う
        integer :: n, m, i, j
        !factorはピボット選択後ガウス消去を行うためにk行以外の行に掛ける係数
        real(real64) :: factor

        n = size(U, 1)
        m = size(U, 2)

        do i = k+1, n

            factor = U(i, k) / U(k, k)
            !factorが決定すると行列Lの下三角の成分が決定する
            L(i, k) = factor

            do j = 1, m

                U(i, j) = U(i, j) - U(k, j) * factor

            end do

        end do

    end subroutine GaussElimination


!-----------------------------------------------
!Lの逆行列計算(下三角)前進代入
!-----------------------------------------------
    subroutine Linverse(L, Linv)

        real(real64), intent(in) :: L(:, :)
        real(real64), intent(out) :: Linv(:, :)

        !nは行列Lのサイズ，Lの成分をL(i,j)とする
        integer :: n, m, i, j, k
        !s(sum)は一時的に和を保存して，最終的に-L(j,j)で割る　数値計算でよく用いる手法
        real(real64) :: s

        n = size(L, 1)
        m = size(L, 2)

        !Linvの中身は後から挿入するとして，一旦全成分を0で統一
        Linv = 0.0d0

        !1列目から考える
        do j = 1, m

            Linv(j, j) = 1.0d0 / L(j, j)

            !上では対角成分を確定させたから，対角以下の成分を考える
            do i = j+1, n

                s = 0.0d0

                do k = 1, i-1

                    s = s + L(i, k) * Linv(k, j)

                end do

                Linv(i, j) = -s / L(i, i)

            end do

        end do

    end subroutine Linverse


!-----------------------------------------------
!Uの逆行列計算(上三角)後退代入
!-----------------------------------------------
    subroutine Uinverse(U, Uinv)

        real(real64), intent(in) :: U(:, :)
        real(real64), intent(out) :: Uinv(:, :)

        integer :: n, m, i, j, k
        real(real64) :: s

        n = size(U, 1)
        m = size(U, 2)

        Uinv = 0.0d0

        !後退代入なのでi,jは後ろから考える

        do j = m, 1, -1

            Uinv(j, j) = 1.0d0 / U(j, j)

            do i = j-1, 1, -1

                s = 0.0d0

                do k = i+1, j

                    s = s + U(i, k) * Uinv(k, j)

                end do

                Uinv(i, j) = -s / U(i, i)

            end do

        end do

    end subroutine Uinverse


end module mod_linear_solver_lu
