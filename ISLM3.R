# ISLM model


# data are quarterly, Jan 2017 to Oct 2019
# all $$ amounts are billions
# all data from 'FRED' Federal Reserve bank St. Louis
#  Data are quarterly from Jan 2017 to Oct 2019

ISLMdata = data.frame(
  
# consumpton  
            Consum = c(13104.43, 13212.5, 13345.07,
                      13586.27, 13728.33, 13939.83,
                      14114.53, 14211.9, 14266.27,
                      14511.17, 14678.2, 14789.27),
                
# GDP, aggregate demand                
                Y = c(19190.431, 19356.649, 19611.704, 19918.91,
                      20163.159, 20510.177, 20749.752, 20897.804,
                      21098.827, 21340.267, 21542.54,  21726.779),

# Government spending                
                G_spend = c(6526.927, 6525.904, 6623.281, 6713.611,
                            6806.73, 6901.58, 6971.018, 7042.209,
                            7149.65, 7292.506, 7359.535, 7424.398),
# Gross domestic savings                
          personalSavings = c(3600.665, 3613.782, 3658.564, 3632.806,
                            3826.156, 3753.84, 3814.944, 3785.88,
                            3909.787, 3866.799, 3826.739, 3840.0),

# M1 money supply, cash and checking deposits              
                M1 = c(3405.462, 3498.1, 3570.162, 3613.338,
                       3639.485, 3658.723, 3688.038, 3719.443, 
                       3743.892, 3796.215, 3867.964, 3950.738),

# M2 money supply  M1 + easily convertible               
                M2 = c(13332.092, 13515.838, 13659.854,
                       13794.4, 13892.846, 14045.685,
                       14186.1, 14275.914, 14464.792,
                       14645.462, 14932.307, 15243.2),

# Government revenue                
                Taxes = c(1987.626, 2003.74, 2042.913, 2042.372,
                         1921.492, 1943.529, 1971.38, 1987.923,
                         2018.649, 2027.559, 2028.389, 2030.0 ),


# velocity of money; how many times $ changes hands/yr              
                Vel_M1 = c(5.623, 5.529, 5.493, 5.513, 5.534,
                           5.605, 5.625, 5.616, 5.636, 5.613,
                           5.564, 5.5),

# net exports $ value of exports - imports                
                Nx = c(-570.922, -583.726, -550.586, -596.11,
                       -628.967, -568.391, -671.353, -684.148,
                       -633.848, -662.66,  -653.032, -577.381),

# consumer price index                
                CPI = c(243.822, 244.054, 245.359, 247.25,
                        249.235, 250.591, 251.883,
                        252.697, 253.275, 255.171,
                        256.325, 257.832),
# interest rate, %                
                Int_r = c(0.7,0.95, 1.153, 1.203, 1.446, 1.737,
                        1.923, 2.22, 2.403, 2.397, 2.19, 1.643),
# libor %
                libor=c(0.830, 1.063, 1.232, 1.330, 1.653,
                        1.972, 2.107, 2.347, 2.498, 2.443,
                        2.179, 1.791),

# investment                
                Invest = c(3140.293, 3167.926, 3225.247, 3262.117,
                           3311.826, 3296.571, 3404.226, 3429.477,
                           3481.088, 3424.653, 3416.18,  3363.567 )
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

MoneyFcn = glm(MoverP ~ Y + Int_rF)
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
yPlot=seq(from=10000, to=32000, by=500)
intRplot = seq(from=0, to=0.5, by= 0.05)


gSpend = G_spend[12]
Nxport = -550
MoverPfixed = MoverP[12]
tax = Taxes[12]/2

plot(yPlot, IS_i(C0, b, yPlot, tax, I0, dy, dInt, gSpend, Nxport),
     type='l', ylab = 'interest rate')
lines(yPlot, LM_i(MoverPfixed, yPlot,M0, d1, d2), 
     ylab=c('yModel'), xlab = 'Y', type='l', col='green')
legend('topleft', c('IS','LM'), lty=1,col=c('black','green'))
grid()

plot(intRplot, LM_Y(MoverPfixed,intRplot, M0, d1, d2),
     ylab = 'GDP', type='l')
lines(intRplot, IS_Y(C0, b, tax, I0, dy, dInt, intRplot,
          gSpend, Nxport))

#Y for various interest rates

# find r for a given Y:

rLow = 0.00001
rHigh = 14.5
#full employment GDP
y_Target = 21500

for (i in 1:20){
  r = (rLow + rHigh)/2
  print(r)
  Yequ = LM_Y(MoverPfixed, r, M0, d1, d2)
  if (abs(Yequ-y_Target) < 1) break
  if (Yequ > y_Target) rLow = r
  else rHigh=r
}
print(paste(i,' iterations, r = ', round(r,3)))

# Assume r  = 0.25, what is equilibrium Y
r = 0.025

equilibY = LM_Y(MoverPfixed, r, M0, d1, d2)
print(equilibY)

# at this equilibY
ISequilib = IS_Y(C0, b, tax, I0, dy, dInt, intRplot, gSpend, Nxport)

# planned inflation:
inflationGap= (ISequilib - y_Target)
print(paste('inflationary gap: ',inflationGap))

# current Y = 22000





