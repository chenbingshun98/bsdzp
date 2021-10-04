require(deSolve)

main <- function(){
  
  ## no intervention
  
  SEIR <- function(time, current_state, params){
    
    with(as.list(c(current_state, params)),{
      N <- S+E+I+R
      dS <- -(beta*S*I)/N
      dE <- (beta*S*I)/N - sigma*E
      dI <- sigma*E - gamma*I - mu*I
      dR <- gamma*I
      dM <- mu*I
      
      return(list(c(dS, dE, dI, dR, dM)))
    })
  }
  
  params <- c(
    beta=0.5, 
    sigma=0.25, 
    gamma=0.2, 
    mu=0.001
  )
  
  initial_state <- c(S=999999, E=1, I=0, R=0, M=0)
  
  times <- 0:365
  
  model <- ode(initial_state, times, SEIR, params)
  
  print(summary(model))
  
  matplot(
    model,
    type="l",
    lty=1,
    main="SEIR model",
    xlab="Time"
  )
  
  legend <- colnames(model)[2:6]
  
  legend("right", legend=legend, col=2:6, lty = 1)
  
  infections <- as.data.frame(model)$I
  
  peak <- max(infections)
  
  match(peak, infections)
  
  print(paste("peak:",peak))
  
  print(paste("peak day:", match(peak, infections)))
  
  
  ### with lockdown
  
  require(deSolve)
  
  SEIR_lockdown <- function(time, current_state, params){
    
    with(as.list(c(current_state, params)),{
      
      beta = ifelse(
        (time <= start_lockdown || time >= end_lockdown),
        0.5, 0.1
      )
      
      N <- S+E+I+R
      dS <- -(beta*S*I)/N
      dE <- (beta*S*I)/N - sigma*E
      dI <- sigma*E - gamma*I - mu*I
      dR <- gamma*I
      dM <- mu*I
      
      return(list(c(dS, dE, dI, dR, dM)))
    })
  }
  
  params <- c(
    sigma=0.25,
    gamma=0.2,
    mu=0.001,
    start_lockdown=90,
    end_lockdown=150
  )
  
  initial_state <- c(S=999999, E=1, I=0, R=0, M=0)
  
  times <- 0:365
  
  model <- ode(initial_state, times, SEIR_lockdown, params)
  
  
  print(summary(model))
  
  matplot(
    model, 
    type="l",
    lty=1, 
    main="SEIR model (with intervention)", 
    xlab="Time"
  )
  
  legend <- colnames(model)[2:6]
  
  legend("right", legend=legend, col=2:6, lty = 1)
  
  infections <- as.data.frame(model)$I
  
  peak <- max(infections)
  
  match(peak, infections)
  
  print(paste("peak:",peak))
  
  print(paste("peak day:", match(peak, infections)))
}

main()

#test
require(deSolve)

SEIR <- function(time, current_state, params){
  
  with(as.list(c(current_state, params)),{
    N <- S+E+I+R
    dS <- -(beta*S*I)/N
    dE <- (beta*S*I)/N - sigma*E
    dI <- sigma*E - gamma*I - mu*I
    dR <- gamma*I
    dM <- mu*I
    
    return(list(c(dS, dE, dI, dR, dM)))
  })
}

params <- c(beta=0.5, sigma=0.25, gamma=0.2, mu=0.001)

initial_state <- c(S=999999, E=1, I=0, R=0, M=0)

times <- 0:365

model <- ode(initial_state, times, SEIR, params)
model <- as.tibble(model)
summary(model)

matplot(model, type="l", lty=1, main="SEIR model", xlab="Time")

legend <- colnames(model)[2:6]

legend("right", legend=legend, col=2:6, lty = 1)

#https://blog.csdn.net/arcers/article/details/104238668
seir<-function(time, state, pars){ 
  with(as.list(c(state, pars)),{ 
    dS <-- S * beta * I/N 
    dE <- S * beta * I/N - E * k 
    dI <- E * k - I * (mu + gamma) 
    dR <- I * gamma
    dN <- dS + dE + dI + dR 
    
    list(c(dS,dE,dI,dR,dN)) 
  }) 
} 

N <- 1.9E8 # 总人口
I0 <- 89 # 期初感染数
E0 <- 0 # 期初潜伏数
RM0 <- 0 # 期初移除数
S0 = N - I0 - RM0 # 期初易感人数
init<-c(S = S0, E = E0, I = I0, R = RM0, N = N)	
time <- seq(0, 150, 1) 
pars<-c( 
  beta = 0.55,	#有效接触率
  k = 1,	#潜伏到感染的转化率 
  gamma = 0.2,	#RECOVERY 
  mu=0.02	#感染期死亡率 
)

res.seir<-as.data.frame(lsoda(y = init, times = time, func = seir, parms = pars)) 
res.seir2<-as.data.frame(ode(y = init, times = time, func = seir, parms = pars)) 

res.seir
res.seir2
res.seir==res.seir2

ggplot(res.seir) +
  geom_line(aes(x = time, y = S, col = '2 易感'))+
  geom_line(aes(x = time, y = E, col = '3 潜伏'))+
  geom_line(aes(x = time, y = I, col = '4 感染'))+
  geom_line(aes(x = time, y = R, col = '5 移除'))+
  geom_line(aes(x = time, y = N, col = '1 人口'))+
  theme_light(base_family = 'Kai') +
  scale_colour_manual("",
                      values=c(
                        "2 易感" = "cornflowerblue", "3 潜伏" = "orange",
                        "4 感染" = "darkred", "5 移除" = "forestgreen", 
                        "1 人口" = "black"
                      )
  ) +
  scale_y_continuous('')

ggplot(model) +
  geom_line(aes(x = time, y = S, col = '2 易感'))+
  geom_line(aes(x = time, y = E, col = '3 潜伏'))+
  geom_line(aes(x = time, y = I, col = '4 感染'))+
  geom_line(aes(x = time, y = R, col = '5 移除'))+
  theme_light(base_family = 'Kai') 

  