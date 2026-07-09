program inkaihou

    use, intrinsic :: iso_fortran_env, only : real64
    use mod_linear_solver_lu, only : matrix_inverse
    implicit none

    !変数の宣言

    integer :: Nx, i, j, n, Nterm

    real(real64) :: L, nu, dt, Tmax, dx, r, t, Bn, An, error_max

    real(real64), allocatable :: x(:), u_old(:), u_new(:), u_exact(:), error(:)

    real(real64), allocatable :: A(:,:), Ainv(:,:)

    !具体的なパラメータの入力

    print *, "区間の長さLは？"
    read(*,*) L

    print *, "拡散係数νは？"
    read(*,*) nu

    print *, "空間の分割数Nxは？"
    read(*,*) Nx

    print *, "時間離散化の幅dtは？"
    read(*,*) dt

    print *, "終了する時間Tmaxは？"
    read(*,*) Tmax

    !空間の分割

    !整数と倍精度実数で計算を行うときは，倍精度実数に合わせてから計算する必要がある．
    dx = L / real(Nx, real64)

    allocate(x(0:Nx))
    !これは各点の座標を表す点だから，ｉループで各点に座標を挿入

    do i = 0, Nx
        x(i) = real(i, real64) * dx
    end do

    allocate(u_old(0:Nx))
    allocate(u_new(0:Nx))
    allocate(u_exact(0:Nx))
    allocate(error(0:Nx))

    allocate(A(Nx-1, Nx-1))
    allocate(Ainv(Nx-1, Nx-1))

    !安定条件の判断（離散化パラメータｒを計算し陽解法が適用できるか判断）
    r = nu * dt / dx**2

    print *, "離散化パラメータrは", r


    !初期条件の設定（問題を参照）

    u_old = 1.0_real64
    U_old(0) = 0.0_real64

    u_new = 0.0_real64

    t = 0.0_real64

    !陰解法で用いる係数行列を作成
    A = 0.0_real64

    do i = 2, Nx-2

        A(i,i-1) = -r
        A(i,i)   = 1.0_real64 + 2.0_real64*r
        A(i,i+1) = -r

    end do

    A(1,1) = 1.0_real64 + 2.0_real64*r
    A(1,2) = -r

    A(Nx-1,Nx-2) = -r
    A(Nx-1,Nx-1) = 1.0_real64 + r

    ! LU分解モジュールを用いて逆行列を計算
    call matrix_inverse(A, Ainv)

    !離散化した時間を順番にたどり，Tmaxにおけるuを計算する

    do while (t < Tmax - 1.0d-11)

        !内部のノード
        do i = 1, Nx-1

            u_new(i) = 0.0_real64

            do j = 1, Nx-1

                u_new(i) = u_new(i) + Ainv(i,j) * u_old(j)

            end do

        end do

        !境界条件

        !x = 0 (Dirichlet)
        u_new(0) = 0.0_real64

        !x = L (Neumann)
        u_new(Nx) = u_new(Nx-1)

        !全ノードの情報を更新したうえで時間を更新
        u_old = u_new
        t = t + dt

    end do


    !解析解の計算

    !フーリエ級数は１００項もあれば十分高い精度が得られるため，打ち切りとして設定
    Nterm = 100

    u_exact = 0.0_real64

    do i = 0, Nx

        do n = 0, Nterm-1

            !式変形で得られた係数を定義

            Bn = (2.0_real64*n + 1) * acos(-1.0_real64) / (2.0_real64*L)

            An = 4.0_real64 / ((2.0_real64*n + 1.0_real64) * acos(-1.0_real64))

            u_exact(i) = u_exact(i) + An * sin(Bn * x(i)) * exp(-nu*Bn*Bn*Tmax)

        end do

    end do


    !誤差の計算
    error_max = 0.0_real64

    do i = 0, Nx

        error(i) = abs(u_old(i) - u_exact(i))

        if (error(i) > error_max) then
            error_max = error(i)

        end if

    end do


    !結果の出力
    open(10, file="inkaihou.csv", status="replace")

    write(10,'(a)') "座標,数値解,解析解,誤差"

    do i = 0, Nx

        write(10,'(4(es16.8,:,","))') &
            x(i), u_old(i), u_exact(i), error(i)

    end do

    close(10)

    print *, "Maximum error =", error_max

    !メモリの解放

    deallocate(x)
    deallocate(u_old)
    deallocate(u_new)
    deallocate(u_exact)
    deallocate(error)
    deallocate(A)
    deallocate(Ainv)

end program inkaihou
