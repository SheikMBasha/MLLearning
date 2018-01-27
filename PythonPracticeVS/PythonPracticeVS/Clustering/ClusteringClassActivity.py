import pandas as pd
import numpy as np
from ggplot import mtcars
from scipy.cluster.hierarchy import dendrogram, linkage
mtcars.head()

fit =  linkage(mtcars, 'ward')
print(fit)