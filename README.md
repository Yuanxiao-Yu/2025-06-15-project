推荐的阅读顺序：

1. 先导入处理完的数据集data_preprocessed.csv

2. 跑一遍code.R
代码的重点在于其中的
# Run ANOVA for each ERP component and measure
run_anova(data_long_uv, "amplitude", "P200", p200_positions)
run_anova(data_long_ms, "latency", "P200", p200_positions)
run_anova(data_long_uv, "amplitude", "N400", n400_positions)
run_anova(data_long_ms, "latency", "N400", n400_positions)
run_anova(data_long_uv, "amplitude", "P600", p600_positions)
run_anova(data_long_ms, "latency", "P600", p600_positions)

3. 为了更容易读懂里面的统计工具，可以参照我的“代码所使用到的统计学方法以及对应参数的通俗易懂解释.docx"

4. 最后所有的数据和结论可以参考“数据汇总.docx”

（注1：可以着重看我打上黄色标记的部分）
（注2: 对png我不是很有信心呜呜呜😭）
谢谢
