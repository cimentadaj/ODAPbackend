bachi_min <- function(p0,target,x,Age){
  p5 <- p0 / 2
  xx <- heapify(x,Age,p0,p5,
                ageMax = 80) # check arg names
  b  <- check_heaping_bachi(Value = xx,
                            Age = Age)
  abs(b - target)
}

x <- seq(10,.01,length=101)
a <- 0:100
p0_target_i <- optimize(bachi_min, interval = c(0,10), target = target, x = x, Age = a)$min
x_target_i  <- heapify(x, p0 = p0_target_i, p5 = p0_target_i / 2)

data(pop1m_pasex, package = "DemoTools")


p0_vec <- rep(NA,5)
heaped_versions <- list()
for (i in 1:5){
  p0_vec[i] <- optimize(bachi_min,
                        interval = c(0,10),
                        target = breaks[i],
                        x = x,
                        Age = a)$min
  xx <- heapify(x, a, p0 = p0_vec[i], p5 = p0_vec[i] / 2,ageMax = 80)
  heaped_versions[[i]] <- tibble(Age = a, Exposures = xx, p0 = p0_vec[i], bachi = breaks[i])
}
heaped_versions <- bind_rows(heaped_versions)

heaped_versions |> 
  group_by(bachi) |> 
  summarize(myers = check_heaping_myers(Value = Exposures, Age = Age))
