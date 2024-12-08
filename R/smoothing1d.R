#' @title smooth1d_chunk
#' @description
#' Smooth a univariate time series, optionally using `weights.` Choose between the super-smoother (`"supsmu"`) method, loess  (`"lowess"` or `"loess"`) , smoothing splines (`"cubicsplines"`), thin-plate splines (`"gam-tp"`), or p-splines  (`"gam-ps"`). Input data may have multiple observations per x coordinate. Output can be for arbitrary x-coordinates (`xout`).
#' @details `"supsmu"` method takes a `smoothing_par` between 0 and 10. `"lowess"` and 
#' @param data_in. `data.frame` with x and y coordinates to fit to. Optionally with `weights`. Should refer to a single subset of data.
#' @param method character. Smoothing method desired. options `"supsmu"` (default),`"lowess"`,`"loess"`,`"cubicspline"`,`"gam-tp"`,`"gam-ps"`
#' @param smoothing_par smoothing parameter, interpretation varies by method, but higher always means smoother.Default 1
#' @param xout numeric vector of coordinates to predict for. Defaults to original unique coordinates.
#' @param xname name of variable containing x coordinate, default `x`
#' @param yname name of variable containing y coordinate, default `y`
#' @importFrom stats supsmu smooth.spline loess
#' @importFrom dplyr case_when
#' @importFrom signal interp1
#' @importFrom mgcv gam

smooth1d_chunk <- function(data_in., 
                     method = c("supsmu","lowess","loess",
                                "cubicspline","gam-tp","gam-ps")[1],
                     smoothing_par = 1,
                     xout = data_in.[["x"]]){
  
  x <- data_in.[[xname]]
  y <- data_in.[[yname]]
  w <- data_in.[["weights"]]
  
  if (method == "supsmu"){
    smoothing_par <- case_when(smoothing_par < 0 ~ 0,
                               smoothing_par > 10 ~ 10,
                               TRUE ~ smoothing_par)
    
    fit <- supsmu(x = x, 
                  y = y, 
                  wt = w,
                  span = "cv", 
                  bass = smoothing_par)
    # ---------------------------------------------------------------#
    # TR: would be bettwe to follow this function with interpolate() #
    # instead of fixing this option?                                 #
    # Then again maybe keep it cuz most methods have predict()?      #
    # ---------------------------------------------------------------# 
    pred <- interp1(fit$x, 
                    fit$y, 
                    xi = xout, 
                    method = 'pchip', 
                    extrap = TRUE)
    data_out <- tibble(!!xname := xout, !!yname := pred)
  }
  
  if (method == "lowess"){
    fit  <- loess(y ~ x, 
                  weights = w, 
                  span = smoothing_par, 
                  degree = 1, 
                  control = loess.control(surface = "direct"))
    pred <- predict(fit, newdata = data.frame(x = xout))
    data_out <- tibble(!!xname := xout, !!yname := pred)
  }
  
  if (method == "loess"){
    fit  <- loess(y ~ x, 
                  weights = w, 
                  span = smoothing_par, 
                  degree = 2, 
                  control = loess.control(surface = "direct"))
    pred <- predict(fit, newdata = data.frame(x = xout))
    data_out <- tibble(!!xname := xout, !!yname := pred)
  }
  
  
  if (setup$Method=="cubicspline") {
    if (is.numeric(smoothing_par)){
      fit  <- smooth.spline(x = x, 
                            y = y, 
                            w = w,
                            spar = smoothing_par)
    } else {
      fit  <- smooth.spline(x = x, 
                            y = y, 
                            w = w,
                            cv = FALSE)
    }
    
    pred <- predict(fit, x = data.frame(x = xout))
    data_out <- data.frame(pred)
    colnames(data_out) <- c(xname, yname)
  }
  
  # covers gam-tp and gam-ps
  if (grepl(method, pattern = "gam")) {
    smoothing_par <- ifelse(is.numeric(smoothing_par), smoothing_par, 1)
    lil_data <- tibble(x = x, y = y, w = w)
    bs <- gsub(method, pattern = "gam-", replacement = "")
    fit <- mgcv::gam(y ~ + s(x, m = smoothing_par), 
                     bs = bs,
                     data = lil_data, 
                     weights = w, 
                     method = "REML", 
                     select = TRUE, 
                     family = "gaussian")
    pred <- predict(fit, newdata = data.frame(x = xout))
    data_out <- tibble(!!xname := xout, !!yname := pred)
  }
  
  data_out
  
}

#' @title smooth1d
#' @description
#' Smooth a univariate time series, optionally using `weights.` Choose between the super-smoother (`"supsmu"`) method, loess  (`"lowess"` or `"loess"`) , smoothing splines (`"cubicsplines"`), thin-plate splines (`"gam-tp"`), or p-splines  (`"gam-ps"`). Input data may have multiple observations per x coordinate. Output can be for arbitrary x-coordinates (`xout`). If grouping variables have been declared, then we use the same parameters for each subset.
#' @details `"supsmu"` method takes a `smoothing_par` between 0 and 10. `"lowess"` and 
#' @param data_in `data.frame` with x and y coordinates to fit to. Optionally with `weights`
#' @param method character. Smoothing method desired. options `"supsmu"` (default),`"lowess"`,`"loess"`,`"cubicspline"`,`"gam-tp"`,`"gam-ps"`
#' @param smoothing_par smoothing parameter, interpretation varies by method, but higher always means smoother.Default 1
#' @param xout numeric vector of coordinates to predict for. Defaults to original unique coordinates.
#' @param xname name of variable containing x coordinate, default `x`
#' @param yname name of variable containing y coordinate, default `y`
#' @importFrom stats supsmu smooth.spline loess
#' @importFrom dplyr case_when
#' @importFrom signal interp1
#' @importFrom mgcv gam
#' @importFrom dplyr mutate filter reframe bind_rows ungroup .data
#' @importFrom purrr map_lgl
#' @importFrom tidyselect all_of
#' @importFrom tidyr unnest
#' @importFrom rlang set_names .data
#' @examples
#' x = c(1950.77104722793, 1951.77104722793, 1952.77104722793, 1952.77104722793, 
#'       1953.77104722793, 1954.77104722793, 1955.77104722793, 1956.77104722793, 
#'       1957.77104722793, 1957.77104722793, 1958.77104722793, 1959.77104722793, 
#'       1963.5, 1964.5, 1965.5, 1966.5, 1967.5, 1968.5, 1969.5, 1970.5, 
#'       1970.5, 1971.5, 1971.5, 1972.5, 1972.5, 1973.5, 1973.5, 1974.5, 
#'       1974.5, 1975.5, 1975.5, 1976.5, 1976.5, 1977.5, 1977.5, 1978.5, 
#'       1978.5, 1979.5, 1979.5, 1980.5, 1980.5, 1981.5, 1981.5, 1982.5, 
#'       1982.5, 1983.5, 1983.5, 1984.5, 1984.5, 1985.5, 1985.5, 1986.5, 
#'       1986.5, 1987.5, 1987.5, 1988.5, 1988.5, 1989.5, 1989.5, 1990.5, 
#'       1990.5, 1991.5, 1991.5, 1992.5, 1992.5, 1993.5, 1993.5, 1994.5, 
#'       1994.5, 1995.5, 1995.5, 1996.5, 1996.5, 1997.5, 1997.5, 1998.5, 
#'       1998.5, 1998.5, 1999.5, 1999.5, 1999.5, 2000.5, 2000.5, 2000.5, 
#'       2001.5, 2001.5, 2001.5, 2001.5, 2002.5, 2002.5, 2002.5, 2003.5, 
#'       2003.5, 2003.5, 2004.5, 2004.5, 2005.5, 2005.5, 2006.5, 2006.5, 
#'       2007.5, 2007.5, 2008.5, 2008.5, 2009.5, 2009.5, 2010.5, 2010.5, 
#'       2011.5, 2012.5, 2013.5, 2014.5, 2015.5, 2016.5, 2017.5)
#' 
#' y = c(5.28312492370605, 5.4010272026062, 5.55507040023804, 5.52694797515869, 
#'       5.65941572189331, 5.81246614456177, 5.95277643203735, 6.07998991012573, 
#'       6.20043277740479, 6.23209381103516, 6.4145884513855, 6.44994592666626, 
#'       5.77428722381592, 6.09462738037109, 6.31580305099487, 5.68929624557495, 
#'       6.34508848190308, 5.67744398117065, 5.6477165222168, 5.12978315353394, 
#'       4.83979654312134, 5.40941429138184, 4.93997049331665, 5.06586456298828, 
#'       4.68799591064453, 4.31546640396118, 4.18193292617798, 3.75800633430481, 
#'       3.82137632369995, 3.17197370529175, 3.41054058074951, 3.08278703689575, 
#'       3.04342699050903, 3.01940298080444, 2.9705445766449, 2.96306347846985, 
#'       2.88018417358398, 2.72204685211182, 2.72689270973206, 3.43744516372681, 
#'       2.88990616798401, 3.12944483757019, 3.18246674537659, 3.20358324050903, 
#'       3.0853967666626, 3.38533687591553, 3.18455958366394, 3.14047956466675, 
#'       3.08752226829529, 3.15941309928894, 3.09168982505798, 3.22912931442261, 
#'       2.95333743095398, 3.05898070335388, 2.91993451118469, 3.29801154136658, 
#'       2.93581032752991, 3.18667984008789, 2.92741537094116, 3.10476756095886, 
#'       2.86983323097229, 3.31816387176514, 2.87090802192688, 3.57006907463074, 
#'       3.20188307762146, 3.38520860671997, 3.1142041683197, 3.05472731590271, 
#'       2.88588929176331, 2.85394668579102, 2.69696426391602, 2.71265292167664, 
#'       2.49935364723206, 2.55814361572266, 2.37988924980164, 2.35105299949646, 
#'       2.32249999046326, 2.21962690353394, 2.46929597854614, 2.43600010871887, 
#'       2.6223669052124, 2.31005716323853, 2.33249998092651, 2.408358335495, 
#'       2.39718627929688, 2.38599991798401, 3, 2.51987075805664, 2.1131649017334, 
#'       2.13849997520447, 2.04657125473022, 2.06265759468079, 2.10050010681152, 
#'       1.98469293117523, 2.09120869636536, 2.2427921295166, 1.97930490970612, 
#'       2.15000796318054, 2.06816005706787, 2.18898129463196, 1.77942955493927, 
#'       1.99617087841034, 1.89081299304962, 2.0644690990448, 1.85119438171387, 
#'       2.03594541549683, 1.83351850509644, 2.02167296409607, 1.89562964439392, 
#'       1.89205574989319, 1.85763072967529, 1.68259692192078, 1.69228148460388, 
#'       1.56845271587372, 1.37076985836029)
#' data_in <- tibble(x=x,y=y,w=1)
#' xout <- 1950:2018 + .5
#' \dontrun{
#'  plot(data_in$x, data_in$y)
#'  lines(xout, smooth1d(data_in, method = "supsmu", 
#'              xout = xout, smoothing_par = .2)$y,  
#'        col = "red")
#'  lines(xout, smooth1d(data_in, method = "loess", 
#'                       xout = xout, smoothing_par = .2)$y,  
#'        col = "blue", lty = "82")
#'  lines(xout, smooth1d(data_in, method = "gam-ps", xout = xout)$y, 
#'        col = "forestgreen" ,lty="82")
#'   lines(xout, smooth1d(data_in, method = "cubicspline", 
#'                        xout = xout, smoothing_par =1)$y, 
#'         col = "purple", lty="22")
#' }
smooth1d <- function(data_in, 
                     method = c("supsmu","lowess","loess",
                                "cubicspline","gam-tp","gam-ps")[1],
                     smoothing_par = 1,
                     xout = data_in[["x"]] |> unique(),
                     by_args = NULL){
  
  if (!(".id" %in% colnames(data_in))) {
    data_in <- data_in |>
      mutate(.id = "all")
  }
  if (!"weights" %in% colnames(data_in)){
    data_in <- data_in |> 
      mutate(weights = 1)
  }
  
  data_out <- data_in |>
    ungroup() |> 
    reframe(
      smooth1d_chunk(data_in. = .data, 
                     method = method,
                     smoothing_par = smoothing_par,
                     xout = xout), 
      .by = all_of(c(".id"))
    ) #|>
    #set_names(c(".id", by_args, "data"))
  
  # data_out <- data_out |>
  #   filter(map_lgl(data, is.data.frame))|>
  #   unnest(.data$data)

return(data_out)
}


