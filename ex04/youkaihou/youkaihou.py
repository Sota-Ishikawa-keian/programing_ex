import pandas as pd
import matplotlib.pyplot as plt

# =====================================================
# CSVファイルの読み込み
# =====================================================
data = pd.read_csv("youkaihou.csv")

# データの取り出し
x = data["座標"]
u_num = data["数値解"]
u_exact = data["解析解"]
error = data["誤差"]

# =====================================================
# 数値解と解析解
# =====================================================
plt.figure(figsize=(8, 5))

plt.plot(x, u_num, "o-", markersize=4, label="Numerical")
plt.plot(x, u_exact, "-", linewidth=2, label="Analytical")

plt.xlabel("x")
plt.ylabel("u")
plt.title("Numerical Solution and Analytical Solution")
plt.grid(True)
plt.legend()

plt.tight_layout()
plt.savefig("youkaihou.png", dpi=300)
plt.savefig("youkaihou.pdf")
plt.show()

# =====================================================
# 誤差分布
# =====================================================
plt.figure(figsize=(8, 5))

plt.plot(x, error, "r-o", markersize=4)

plt.xlabel("x")
plt.ylabel("Error")
plt.title("Error Distribution")
plt.grid(True)

plt.tight_layout()
plt.savefig("erroryou.png", dpi=300)
plt.savefig("erroryou.pdf")
plt.show()
