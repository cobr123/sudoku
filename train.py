from fastai.tabular.all import *
from pathlib import Path


def train(df, idx):
    splits = RandomSplitter(valid_pct=0.2)(range_of(df))

    cat_names = []
    y_name = ''

    for i in range(81):
        name = 'cell_' + str(i)
        if i == idx:
            y_name = name
        else:
            cat_names.append(name)

    to = TabularPandas(df, procs=[Categorify],
                       cat_names=cat_names,
                       y_block=CategoryBlock,
                       y_names=y_name,
                       splits=splits)

    print(to.xs.iloc[:2])
    cats = to.procs.categorify
    print(cats[cat_names[0]])

    dls = to.dataloaders(bs=64)
    dls.show_batch()

    learn = tabular_learner(dls, metrics=accuracy)
    learn.fit_one_cycle(4)
    learn.show_results()
    learn.export('./models/model_' + str(idx) + '.pkl')

    print('Saved model idx ' + str(idx))
    print(df.iloc[0])
    row, clas, probs = learn.predict(df.iloc[0])
    print(row)
    print(clas)
    print(probs)
    #
    # test_df = df.copy()
    # test_df.drop([y_name], axis=1, inplace=True)
    # dl = learn.dls.test_dl(test_df)

    # print(learn.get_preds(dl=dl))


path = Path('./data_set')
# print(path.ls())


for idx in range(81):
    filePath = path / ('all_' + str(idx) + '.csv')
    print(filePath)
    df = pd.read_csv(filePath)
    # print(df.head())
    train(df, idx)
