import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
from wordcloud import WordCloud
from IPython.display import display, Markdown


def Display(df):
    return display(Markdown(df))


def total_variable(df, var):
    data = (
        df.groupby(var, as_index=False)["Rating"]
        .agg(["sum", "count", "mean"])
        .reset_index()
        .sort_values("count", ascending=False)
    )
    data = data.assign(PROPORCAO=data["sum"] / data["count"])
    return data


def time_plot(df, var):
    total_hour = total_variable(df, var)
    fig, ax = plt.subplots(1, 2)
    ax[0].set_title("Totais de Review")
    ax[0].set_ylabel("Total")
    ax[1].set_ylabel("Média")
    ax[1].set_title("Média do Review")
    sns.lineplot(data=total_hour, x=var, y="count", ax=ax[0])
    sns.lineplot(data=total_variable(df, var), x=var, y="mean", ax=ax[1])
    fig.show()


def my_wc(df, Filter=0, var="Review"):
    if Filter == "podre":
        data = df[df["Rating"] == 1][var]
    else:
        data = df[df["Rating"] > Filter][var]
    wc = WordCloud(
        width=1000, height=1000, background_color="white", max_words=500
    ).generate(" ".join(data))
    plt.axis("off")
    plt.imshow(wc)
