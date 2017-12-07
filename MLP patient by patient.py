import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.cluster import KMeans
from sklearn.metrics import normalized_mutual_info_score as nmi
from sklearn.preprocessing import StandardScaler
from sklearn.neural_network import MLPClassifier
from sklearn.preprocessing import OneHotEncoder
from sklearn.metrics import precision_score, recall_score, f1_score

# Poly features
def poly(d):
    for i in ['FSC.H', 'SSC.H', 'FL1.H', 'FL2.H', 'FL3.H', 'FL4.H']:
        for j in ['FSC.H', 'SSC.H', 'FL1.H', 'FL2.H', 'FL3.H', 'FL4.H']:
            d[i+j] = d[i] * d[j]
    return d

# Split into a training set and testing set to make sure that
# the MLP is not overfitting by simply memorizing the data
def split(data,y, probability):
    # Make a vector counting [1,2,3..., num_of _features]
    shuffled_indexes = np.arange(len(y))
    # Shuffle that shit
    np.random.shuffle(shuffled_indexes)
    #making sure the lengths of target and features match up
    if len(data) != len(y):
        print('you fucked up')
        return('You Fucked Up')
    # non-intuitive, library-specific code doing the actual splitting
    xtrain = data.iloc[shuffled_indexes[:int(len(data) * probability)]]
    ytrain = y[[shuffled_indexes[:int(len(y) * probability)]]]
    xtest = data.iloc[shuffled_indexes[int(len(data) * probability):]]
    ytest = y[[shuffled_indexes[int(len(y) * probability):]]]
    return(xtrain, ytrain, xtest, ytest)


for i in range(1,13):
    data = pd.read_csv(open('/home/persimmon/Documents/project/everyone.csv'), index_col=0)
    data = data.iloc[np.where(data['patient'] == i)]
    # removing outliers,comment if you dont want to
    data = data.iloc[np.where(data['Targets'] != 0)]

    y = np.array(data['Targets'])
    del data['Targets']
    del data['patient']
    X = data

    x_train, y_train, x_test, y_test = split(X, y, .8)

    ss = StandardScaler()
    # Multilayer perceptron with 15 layers, 20 cells, alphs is regularization, verbose makes it dictate how
    # its doing so far but sows it down
    mlp = MLPClassifier([20] * 15, alpha=.0001, verbose=False, learning_rate_init=.0001)
    mlp.fit(ss.fit_transform(x_train), y_train)

    km = KMeans(4)
    km.fit_transform(poly(x_train))
    print('patient',i,' f Score MLP: ', f1_score(y_test, mlp.predict(ss.transform(x_test)), average='micro'))
    print('patient', i, 'MLP: ', nmi(y_test, mlp.predict(ss.transform(x_test))), 'Kmeans NMI: ', nmi(y_test, km.predict(poly(x_test))))


# # read that bitch and assign X feature matrix and y targets
# data = pd.read_csv(open('/home/persimmon/Documents/project/everyone.csv'), index_col=0)
# # data = data.iloc[np.where(data['patient'] == 10)]
# y = np.array(data['Targets'])
# del data['Targets']
# del data['patient']
# X = data
# print(data.head())
#
# x_train, y_train, x_test, y_test = split(X, y, .8)
# ss = StandardScaler()
# x_train = ss.fit_transform(x_train)
# x_test = ss.transform(x_test)
# # Multilayer perceptron with 15 layers, 20 cells, alphs is regularization, verbose makes it dictate how
# # its doing so far but sows it down
# mlp = MLPClassifier([20]*15, alpha=.001,verbose=True,learning_rate_init=.001)
# mlp.fit(x_train, y_train)
# print('MLP:     ',nmi(y_test, mlp.predict(x_test)))
# print(precision_score(y_test, mlp.predict(x_test), average='micro'))
# print(recall_score(y_test, mlp.predict(x_test), average='micro'))
# print(f1_score(y_test, mlp.predict(x_test), average='micro'))
#
# km = KMeans(5)
# km.fit_transform(x_train)
# thing = km.predict(x_test)
# print('kmeans NMI:     ',nmi(y_test, thing))

# Trying different scaling/poly-featues, confirms our results

# ss = StandardScaler()
# X = ss.fit_transform(X)
# km = KMeans(5)
# km.fit_transform(X)
# thing = km.predict(X)
# print('Scaled NMI:      ', nmi(y, thing))
#
#
# ss = StandardScaler()
# X = poly(data)
# km = KMeans(5)
# km.fit_transform(X)
# thing = km.predict(X)
# print('poly, not scaled NMI:        ', nmi(y, thing))

# ss = StandardScaler()
# X = ss.fit_transform(poly(data))
# km = KMeans(5)
# km.fit_transform(X)
# thing = km.predict(X)
# print('KMeans, poly,scaled NMI:     ', nmi(y, thing))
#
#
# ss = StandardScaler()
# X = ss.fit_transform(poly(data))
# km = KMeans(5)
# km.fit_transform(X)
# thing = km.predict(X)
# print('poly,scaled NMI:     ', nmi(y, thing))
