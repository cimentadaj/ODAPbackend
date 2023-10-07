#' @param age_out character, either `"single"`, or `"abridged"`
lt_flexible <- function(Deaths    = Deaths, # replace with NULL. this is for demonstration purposes
                        Exposures = Exposures,
                        Age       = Age,
                        
                        # recall all of these are passed in from the app, which will contain
                        # its own default values.
                        OAnew     = 100,
                        age_out = "single", # CHECK! This is for option number 2 
                        etrapFrom = 80,
                        extrapFit = Age[Age >= 60], # maybe somehow modify the argument? Not sure if needed
                        radix     = 1e+05,
                        extrapLaw = NULL,
                        SRB       = 1.05,
                        a0rule    = "ak",
                        axmethod  = "un",
                        Sex       = "m") {
  # TR: no need to determine extrapLaw here, it happens
  # natively in the lt functions.
  
  
  
  age_in <- case_when(is_single(Age) ~ "single",
                      is_abridged(Age) ~ "abridged",
                      TRUE ~ "problem")
  
  # TR: this can become the checker function I guess
  if (age_in == "problem"){
    stop(
      "Age groups appear irregular. Only single or standard abrdiged ages are supported now"
    )
  }
  
  # age_in and out_out both abridged
  if(age_in == "abridged" & age_out == "abridged") {
    # TR possibly more args to pass, or different arg management;
    # for instance, construct a completed list of args
    # and execute the function using do.call()
    data_out <- lt_abridged(Deaths  = Deaths,
                            Exposures  = Exposures,
                            Age        = Age,
                            AgeInt     = AgeInt,
                            OAnew      = OAnew,  
                            extrapFrom = extrapFrom,
                            extrapFit  = extrapFit,
                            radix      = radix,
                            extrapLaw  = extrapLaw,
                            SRB        = SRB,
                            a0rule     = a0rule,
                            axmethod   = axmethod,
                            Sex        = Sex)
  }
  
  # age_in abridged and age_out single
  if (age_in == "abridged" & age_out == "single"){
    data_out <- lt_abridged2single(Deaths     = Deaths,
                                   Exposures  = Exposures,
                                   Age        = Age,
                                   AgeInt     = AgeInt,
                                   OAnew      = OAnew,  
                                   extrapFrom = extrapFrom,
                                   extrapFit  = extrapFit,
                                   radix      = radix,
                                   extrapLaw  = extrapLaw,
                                   SRB        = SRB,
                                   a0rule     = a0rule,
                                   axmethod   = axmethod,
                                   Sex        = Sex)
  }
  
  # age_in single, calculate no matter whether we 
  # keep single ages or want abridged output; if single
  # data_out doesn't change again
  if (age_in == "single"){
    # useful in case we use lt_single_mx()
    Mx_emp <- Deaths/ Exposures
    # Don't check age_out yet here, because the abridge function requires a 
    # precalculated lifetable, see below
    # TR same story; arg management should be complete and strategic
    data_out <- lt_single_mx(nMx        = Mx_emp,
                             Age        = Age,
                             OAnew      = OAnew,
                             extrapFrom = extrapFrom,
                             extrapFit  = extrapFit, # should we change it here too to 1 year intervals?
                             radix      = radix,
                             extrapLaw  = extrapLaw,
                             SRB        = SRB,
                             a0rule     = a0rule,
                             axmethod   = axmethod,
                             Sex        = Sex)
    
  }
  
  # final case, age_in single and age_out abridged,
  # requires the precalculated single-age lifetable
  if (age_in == "single" & age_out == "abridged"){
    
    data_out <- lt_single2abridged(lx  = data_out$lx, 
                                   nLx = data_out$nLx, 
                                   ex  = data_out$ex, 
                                   Age = data_out$Age)
    
  }
  
  # now all cases handled
  return(data_out)
  
}
