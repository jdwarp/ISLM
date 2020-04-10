# ISLM model


# data are quarterly, Jan 2017 to Oct 2019
# all $$ amounts are billions
# all data from 'FRED' Federal Reserve bank St. Louis
#  Data are quarterly from Jan 2017 to Oct 2019

# GDP due to Corona Virus, COVID19, now projected to be a
# contraction of between 8 and 15%.  Assume 12% contraction

# modify first Q of 2020 to reflect this, then pursue
# policy

ISLMdata = data.frame(
  
# consumpton  
            Consum = c(13104.43, 13212.5, 13345.07,
                      13586.27, 13728.33, 13939.83,
                      14114.53, 14211.9, 14266.27,
                      14511.17, 14678.2, 14789.27,
                      13024.00, 12500.0),
                
# GDP, aggregate demand                
                Y = c(19190.431, 19356.649, 19611.704, 19918.91,
                      20163.159, 20510.177, 20749.752, 20897.804,
                      21098.827, 21340.267, 21542.54,  21726.779,
                      19100.00, 18700.0),

# Government spending                
                G_spend = c(6526.927, 6525.904, 6623.281, 6713.611,
                            6806.73, 6901.58, 6971.018, 7042.209,
                            7149.65, 7292.506, 7359.535, 7424.398,
                            9500.00, 9600.99),
# Gross domestic savings                
          personalSavings = c(3600.665, 3613.782, 3658.564, 3632.806,
                            3826.156, 3753.84, 3814.944, 3785.88,
                            3909.787, 3866.799, 3826.739, 3840.0,
                            3072.00, 2900.0),

# M1 money supply, cash and checking deposits              
                M1 = c(3405.462, 3498.1, 3570.162, 3613.338,
                       3639.485, 3658.723, 3688.038, 3719.443, 
                       3743.892, 3796.215, 3867.964, 3950.738,
                       4000.00, 4100.00),

# M2 money supply  M1 + easily convertible               
                M2 = c(13332.092, 13515.838, 13659.854,
                       13794.4, 13892.846, 14045.685,
                       14186.1, 14275.914, 14464.792,
                       14645.462, 14932.307, 15243.2,
                       13000.00, 12500.00),

# Government revenue                
                Taxes = c(1987.626, 2003.74, 2042.913, 2042.372,
                         1921.492, 1943.529, 1971.38, 1987.923,
                         2018.649, 2027.559, 2028.389, 2030.0,
                         1725.0, 1700.00),


# velocity of money; how many times $ changes hands/yr              
                Vel_M1 = c(5.623, 5.529, 5.493, 5.513, 5.534,
                           5.605, 5.625, 5.616, 5.636, 5.613,
                           5.564, 5.5,
                           4.5, 4.4),

# net exports $ value of exports - imports                
                Nx = c(-570.922, -583.726, -550.586, -596.11,
                       -628.967, -568.391, -671.353, -684.148,
                       -633.848, -662.66,  -653.032, -577.381,
                       -500.00, -475),

# consumer price index                
                CPI = c(243.822, 244.054, 245.359, 247.25,
                        249.235, 250.591, 251.883,
                        252.697, 253.275, 255.171,
                        256.325, 257.832,
                        260.0, 255.0), #slight increase in CPI
# interest rate, %                
                Int_r = c(0.7,0.95, 1.153, 1.203, 1.446, 1.737,
                        1.923, 2.22, 2.403, 2.397, 2.19, 1.643,
                        0.00, 0.00),
# libor %
                libor=c(0.830, 1.063, 1.232, 1.330, 1.653,
                        1.972, 2.107, 2.347, 2.498, 2.443,
                        2.179, 1.791,
                        0.00, 0.00),

# investment                
                Invest = c(3140.293, 3167.926, 3225.247, 3262.117,
                           3311.826, 3296.571, 3404.226, 3429.477,
                           3481.088, 3424.653, 3416.18,  3363.567,
                           2700.00, 2650.00)
)

attach(ISLMdata)

pIndex = CPI/CPI[1] # normalize CPI
MPC = Consum/Y  # marginal propensity to consume, quarterly
interestRate = libor
MoverP = M1/pIndex # constant $ money supply Gross $$ divided by
                   # price index

# IS - investment saving..
#  Y = C + I + G - Nx   macro economic description
#      C = C0 + b*(Y-T) # T is taxes
#      I = I0 + dY * Y + dInt * interest
#      G, Nx are exogenous


# LM - liquidity preference for money
# M/P = M0 + m1 * Y + m2 * interest     

#IS curve: 
# Y = (C0 + b*(Y-T) + I0 + dY*Y +dInt*interest + G + Nx )
# write two IS functions:
# IS_Y, solve for y in the IS descripton
# IS_i, solve for interest in the IS description

# LM curve:
# write two LM functions:
# LM_Y, solve for Y in the M/P equation
# LM_i, solve for i in the M/P equation

# Quesions:
# 1. given the last values of the variables, what are the 
#    equilibrium levels of interest rate, and GDP.   
#    If these are difference from the present levels,  
#    what does this model suggest about the US economy?
#
# 2. Policy- the COVID19 virus will likely reduce GDP by 1 or 2 %
#    change Nx by about 5% (import less).  Given this,
#    What policy measures do the IS-LM model suggest as a way to
#    increase GDP?   

#  FUNCTIONS --------------------------------------------------
# consumption function
# consumption is a function of income, Y

consumpFcn=glm(Consum~(Y-Taxes)) # Y-Taxes is disosable income
summary(consumpFcn)
#get the coefficients for later use
C0 = as.numeric(consumpFcn$coefficients[1])
b = as.numeric(consumpFcn$coefficients[2])


# investment function
# investment is a function of interest rate
#

investFcn=glm(Invest ~ Y + interestRate)
summary(investFcn)
I0 = as.numeric(investFcn$coefficients[1]) #fixed investment
dY = as.numeric(investFcn$coefficients[2])
dInt = as.numeric(investFcn$coefficients[3])

MoneyFcn = glm(MoverP ~ Y + interestRate)
M0 = as.numeric(MoneyFcn$coefficients[1])
d1 = as.numeric(MoneyFcn$coefficients[2])
d2 = -as.numeric(MoneyFcn$coefficients[3])


# IS estimate of Y
IS_Y = function(C0, b, tax, I0, dy, dInt, intR, G, Nx){
  Ypred = (C0-b*tax+I0 + dInt*intR+G+Nx)/(1-b-dY)
  return(Ypred)
}

# IS estimate of interest
IS_i = function(C0, b, Y, tax, I0, dy, dInt, G, Nx){
  intRpred = (Y*(1.-b-dY)-C0 +b*tax -I0 -G - Nx)/dInt
  return(intRpred)
}

# LM estimate of Y
LM_Y = function(MoverP, intR, M0, d1, d2){
  Y = (MoverP -M0 - d2*intR)/d1
}

# LM estimate of i
LM_i = function(MoverP, Y, M0, d1, d2){
  i = (MoverP - M0-d1*Y)/d2
  return(i)
}

#----------------------------------------------------------
yPlot=seq(from=10000, to=36000, by=500)
intRplot = seq(from=-12, to=4, by= 0.2)

lastN = length(G_spend)
gSpend = G_spend[lastN]



Nxport = Nx[lastN]
tax = Taxes[lastN]
Invest0 = I0
Consmp0 = C0
plot(yPlot, IS_i(Consmp0, b, yPlot, tax, Invest0, dy, dInt, gSpend, Nxport),
     type='l', ylab = 'interest rate', ylim=c(-12, 5))

Nxport = -500
Invest0=500 # fixed investment
Consmp0 = -280

lines(yPlot, IS_i(Consmp0, b, yPlot, tax, Invest0, dy, dInt, gSpend, Nxport),
      type='l', col='grey')

lines(yPlot, LM_i(MoverPfixed, yPlot,M0, d1, d2), 
     ylab=c('yModel'), xlab = 'Y', type='l', col='green')
abline(v=22000, lty=2, col='red')
legend('left', c('IS','LM','Full E'),
       col=c('black','green','red'), lty=c(1,1,3))
grid()


##  

MoP = MoverP[lastN]
plot(intRplot, LM_Y(MoP,intRplot, M0, d1, d2)/1.e3,
     ylab = 'GDP, T$', type='l',col='black', ylim=c(20,40))

MoP = 4500
lines(intRplot, LM_Y(MoP,intRplot, M0, d1, d2)/1.e3,
      ylab = 'GDP, T$', type='l',col='grey')


tX = Taxes[lastN]
lines(intRplot, IS_Y(C0, b, tX, I0, dy, dInt, intRplot,
          gSpend, Nxport)/1.e3, col='green')
tX = 7000
lines(intRplot, IS_Y(C0, b, tX, I0, dy, dInt, intRplot,
                     gSpend, Nxport)/1.e3, col='grey')

abline(h=22000/1.e3, lty=2, col='red')
legend('topleft',c('LM', 'IS', 'Full E'), 
       col=c('black','green','red'), lty=c(1,1,2))
grid()






