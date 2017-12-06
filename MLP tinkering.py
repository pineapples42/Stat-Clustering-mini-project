import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.cluster import KMeans
from sklearn.metrics import normalized_mutual_info_score as nmi
from sklearn.preprocessing import StandardScaler
from sklearn.neural_network import MLPClassifier
from sklearn.preprocessing import OneHotEncoder

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
        return('You Fucked Up')
    # non-intuitive, library-specific code doing the actual splitting
    xtrain = data.iloc[shuffled_indexes[:int(len(data) * probability)]]
    ytrain = y[[shuffled_indexes[:int(len(y) * probability)]]]
    xtest = data.iloc[shuffled_indexes[int(len(data) * probability):]]
    ytest = y[[shuffled_indexes[int(len(y) * probability):]]]
    return(xtrain, ytrain, xtest, ytest)

#read that bitch and assign X feature matrix and y targets
data = pd.read_csv(open('/home/persimmon/Documents/project/everyone.csv'), index_col=0)
y = np.array(data['Targets'])
del data['Targets']
X = data

# One hot encoder, doesn't really work :( leave commented
# Should improve the results when we do get it working

# ohe = OneHotEncoder().fit_transform(y.reshape(-1,1))
# print(np.array(ohe))


x_train, y_train, x_test, y_test = split(X, y, .8)

# Multilayer perceptron with 15 layers, 20 cells, alphs is regularization, verbose makes it dictate how
# its doing so far but sows it down
mlp = MLPClassifier([20]*15, alpha=.0001,verbose=True,learning_rate_init=.0001)
mlp.fit(x_train, y_train)
print('MLP:     ',nmi(y_test, mlp.predict(x_test)))

km = KMeans(5)
km.fit_transform(x_train)
thing = km.predict(x_test)
print('Vanilla NMI:     ',nmi(y_test, thing))

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
