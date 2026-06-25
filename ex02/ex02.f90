program ex02
    implicit none

    !ボタンの数
    integer :: N
    !ボタンを押す回数(0からスタート)
    integer :: push = 0
    !光っているボタンの番号
    integer :: light = 1
    !a1~anを成分にとる配列(サイズはNの入力によって決定)
    integer, allocatable :: a(:)
    !配列の成分を入力するためにdoループを回す=i(1~N)
    integer :: i
    !光ったボタンを1，光っていないボタンを0としてループに入るかどうかを判別(サイズはNの入力によって決定)
    integer, allocatable :: done(:)

    !Nの値(ボタンの数を標準入力によって確定)
    read *, N
    !未定だった配列のサイズがNに決定
    allocate(a(N), done(N))

    !doループで１～Nの次光るボタンの数字を格納
    do i = 1, N
        read  *, a(i)
    end do

    !まだ過去に光ったボタンはないので0で統一
    done = 0

    !--------------------------------------------------------------------------------------------------
    !重要な解釈→ボタンを押すループが終わるのは…
    !・同じボタンが2回目の発光をして無限ループに突入する場合
    !・ボタンを押して更新した後光っているボタンが２の場合

    do

        !今光っているボタンが過去に光ったことがある場合はpushを-1にしてループを抜ける
        if (done(light) == 1) then

            push = -1
            exit

        end if

        !ループを抜けなければ光っているボタンを押し，done，light，pushを更新
        done(light) = 1
        light = a(light)
        push = push + 1

        !更新後，光っているボタンが2であればループを抜ける
        if (light == 2) exit

        !2つのifいずれにも該当しない場合ループを繰り返す

    end do

    !ループを抜けた状態のpushを表示
    print *, push

end program ex02
