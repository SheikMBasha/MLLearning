       mae        mse       rmse       mape 
0.15388315 0.06503103 0.25501183 0.91108427 
> regr.eval(test_target, pred_test)
      mae       mse      rmse      mape 
0.2203253 0.1683485 0.4103029 1.1221224 

########################
10,10

      mae        mse       rmse       mape 
0.11273081 0.03794958 0.19480653 0.61562160 
> regr.eval(test_target, pred_test)
      mae       mse      rmse      mape 
0.2042327 0.1393851 0.3733431 1.0606959 
> 
#####################
10,20
> regr.eval(train_target, pred_train)
       mae        mse       rmse       mape 
0.08682808 0.02376715 0.15416599 0.51802800 
> regr.eval(test_target, pred_test)
       mae        mse       rmse       mape 
0.17853869 0.09792432 0.31292861 0.96759038 
> 
###################
10,30

       mae        mse       rmse       mape 
0.07992567 0.02003510 0.14154539 0.49486268 
> regr.eval(test_target, pred_test)
       mae        mse       rmse       mape 
0.17292591 0.09113586 0.30188716 0.97715534 
> 
###########################################
20,30
> regr.eval(train_target, pred_train)
       mae        mse       rmse       mape 
0.07594644 0.01772898 0.13315020 0.47331585 
> regr.eval(test_target, pred_test)
       mae        mse       rmse       mape 
0.16630178 0.08625397 0.29369027 0.94024796 
###########################################
30,30
       mae        mse       rmse       mape 
0.07594644 0.01772898 0.13315020 0.47331585 
> regr.eval(test_target, pred_test)
       mae        mse       rmse       mape 
0.16630178 0.08625397 0.29369027 0.94024796 
###############################################
50,40
> regr.eval(train_target, pred_train)
       mae        mse       rmse       mape 
0.07012095 0.01512949 0.12300199 0.43689128 
> regr.eval(test_target, pred_test)
       mae        mse       rmse       mape 
0.15946579 0.07976437 0.28242586 0.89900261 