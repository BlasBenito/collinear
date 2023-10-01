Timothy Schwinghamer (he/him) @schwinghamer@mstdn.social

@blasbenito ordinarily, I use caret::dummyVars and predict() to transform the categorical variables into subsets of binary indicator variables. After replacing all the NA and NaN and Inf values with column means, I use dplyr::findLinearCombos. After removing those variables, I use faraway::vif until no value of the VIF is greater than ten.

@MikeMahoney218@fosstodon.org

@blasbenito I've come around to ignoring it entirely ;) 

Now that said, I'm only rarely doing causal inference -- I'm usually focused on prediction or exploration -- which means I don't often care about the precision of my coefficients. But I've also found these two papers really persuasive --

https://psyarxiv.com/mv2wx/
https://quod.lib.umich.edu/cgi/p/pod/d
