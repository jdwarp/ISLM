# ISLM model, Keynesian stuff


# data are quarterly, Jan 2017 to Oct 2019
# all $$ amounts are billions
# all data from 'FRED' Federal Reserve bank St. Louis
#  Data are quarterly from Jan 2017 to Oct 2019

ISLMdata = data.frame(
  
  # consumpton  
  Consum = c(13104.43333, 13212.5, 13345.06667,
             13586.26667, 13728.33333, 13939.83333,
             14114.53333, 14211.9, 14266.26667,
             14511.16667, 14678.2, 14789.26667),
  
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
  M1 = c(3405.461538, 3498.1, 3570.161538, 3613.338462,
         3639.484615, 3658.723077, 3688.038462,
         3719.442857, 3743.891667, 3796.215385,
         3867.964286, 3950.738462),
  
  # M2 money supply  M1 + easily convertible               
  M2 = c(13332.09231, 13515.83846, 13659.85385,
         13794.4, 13892.84615, 14045.68462,
         14186.1, 14275.91429, 14464.79167,
         14645.46154, 14932.30714, 15243.2),
  
  # Government revenue                
  Taxes = c(1987.626, 2003.74, 2042.913, 2042.372,
            1921.492, 1943.529, 1971.38, 1987.923,
            2018.649, 2027.559, 2028.389, 2030.0 ),
  
  
  # velocity of money; how many times $ changes hands                
  Vel_M1 = c(5.623, 5.529, 5.493, 5.513, 5.534,
             5.605, 5.625, 5.616, 5.636, 5.613,
             5.564, 5.5),
  
  # net exports $ value of exports - imports                
  Nx = c(-570.922, -583.726, -550.586, -596.11,
         -628.967, -568.391, -671.353, -684.148,
         -633.848, -662.66,  -653.032, -577.381),
  
  # consumer price index                
  CPI = c(243.822, 244.0543333, 245.359, 247.25,
          249.2346667, 250.591, 251.8826667,
          252.6973333, 253.2753333, 255.1706667,
          256.3246667, 257.8323333),
  # interest rate, %                
  fedRate = c(0.7,0.95, 1.153, 1.203, 1.446, 1.737,
              1.923, 2.22, 2.403, 2.397, 2.19, 1.643),
  # libor %
  libor=c(0.829557031, 1.06296541, 1.231578281,
          1.330114127, 1.653367302, 1.971670645,
          2.107461406, 2.34698, 2.49826381,
          2.442525738, 2.178625385, 1.791037656 ),
  
  # investment                
  Invest = c(3140.293, 3167.926, 3225.247, 3262.117,
             3311.826, 3296.571, 3404.226, 3429.477,
             3481.088, 3424.653, 3416.18,  3363.567 )
)

attach(ISLMdata)

pIndex = CPI/CPI[1] # normalize CPI
MPC = Consum/Y  # marginal propensity to consume, quarterly
interestRate = libor
MoverP = M1/pIndex # constant $ money supply Gross $$ supply divided by
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
# IS_Y, solve for y in the IS equation
# IS_i, solve for interest in the IS equation

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
# 2. Policy- the COVD19 virus will likely reduce GDP by 1 or 2 % and
#    change Nx by about 5% (we import less).  Given this,
#    What policy measures do the IS-LM model suggest as a way to
#    increase GDP?    

#  FUNCTIONS --------------------------------------------------
# consumption function
# consumption is a function of income, Y

consumpFcn=glm(Consum~(Y-Taxes)) # Y-Taxes is disposable income
summary(consumpFcn)
#get the coefficients for later use
C0 = as.numeric(consumpFcn$coefficients[1])
b = as.numeric(consumpFcn$coefficients[2])

summary(consumpFcn)

investFcn = glm(Invest ~ Y + interestRate)
summary(investFcn)
I0 = investFcn$coefficients[1]
dY = investFcn$coefficients[2]
dInt = investFcn$coefficients[3]

moneyFcn = glm(MoverP ~ Y + interestRate)
summary(moneyFcn)
M0 = moneyFcn$coefficients[1]
m1 =moneyFcn$coefficients[2]
m2 = moneyFcn$coefficients[3]

  
IS_Y = glm()