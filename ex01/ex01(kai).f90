program main
    implicit none

    !硬貨を持っている枚数(入力値)
    integer :: A, B, C
    !硬貨を実際に使う枚数(制御構文ループで０枚から試していく)
    integer :: d, e, f
    !組み合わせて目指す合計金額(入力値)
    integer :: X
    !試している合計金額
    integer :: total
    !何通りあるか
    integer :: ways = 0

    print *, "A,B,C は 0~50 の整数"
    print *, "A+B+C >= 1"
    print *, "X は 50~20000 の50倍数"
    print *, "A B C X を入力してください"

    !持っている硬貨の枚数,目指す合計金額を入力
    read *, A, B, C, X

    if (A<0 .or. A>50 .or. B<0 .or. B>50 .or. C<0 .or. C>50 .or. &
        A+B+C < 1 .or. &
        X<50 .or. X>20000 .or. &
        mod(X, 50) /= 0 ) then
            print *, "入力値が制約を満たしていません"

    else
        !ネスト構造(500円，100円，50円を使う枚数の枝分かれ)
        do d = 0, A
            do e = 0, B
                do f = 0, C

                    total = d*500 + e*100 + f*50

                    !合計金額が一致するような組み合わせを見つけると通りを1増やす
                    if (total == X) then
                        ways = ways + 1
                    end if

                end do
            end do
        end do

    !答えを表示
    print *, ways

    end if

end program
