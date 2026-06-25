program ex01
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

    !持っている硬貨の枚数,目指す合計金額を入力
    read *, A, B, C, X

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

end program
