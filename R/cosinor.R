ggplot(act) + geom_line(aes(timestamp, pim))

y_real = 10

T = 24
omega = (360/T)*t_i

#y_i = M + A* cos(omega + acrofase) + e_i

beta = A*cos(acrofase)
gamma = =A*sen(acrofase)

y_i = M + beta*cos(omega) + gamma(sen(omega)) + e_i

