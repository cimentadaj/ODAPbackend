
# you can load with library(), if you're
# certain to have the up-to-date version
devtools::load_all()


# user_file is presumed given from the UI;
# this way of getting data into memory is optional;
# you might handle data loading more directly using
# some shiny tools. In this case, let's expect
# a data.frame called data_in, like the one we see here;


data_in <- readr::read_csv(system.file("extdata",
                                       "abridged_data.csv",
                                       package="ODAPbackend"))
# character string of file name, presumed in data/ folder
# returns data.frame
# data_in <- read_data(user_file)

# Note, column AgeInt is actually optional
# Note, column Mx_emp is currently created by
# read_data(), but it could be created on the 
# fly elsewhere when needed, we don't necessarily
# need to insist on having it now.

# This is data.frame in, data.frame out;
# the created data.frame is a table of checks, pass status, and messages for any failures. The failure messages should be displayed usefully somewhere. Checks that 
# don't pass should necessitate action by users.
initial_data_checks <- check_data(data_in)

# data.frame in, list of three ggplot objects out.
# We can toggle which plots are created with arguments,
# but I suppose we can just produce all 3. Not sure whether
# to show all three immediately in sequence, or just show one
# at a time, and have the user optionally flip through them.

initial_plots <- plot_initial_data(data_in)

# do assorted diagnostics, so far only heaping indices offered
# this is data.frame in data.frame out; the results are a table
# of index values (Bachi and Myers indices for single age data; 
# sawtooth and roughness for abridged / 5-year data), and no user control is given.
# That is, only these methods are given, and with default age ranges.

heaping_exposure <- check_heaping_general(data_in, "Exposures")
heaping_deaths   <- check_heaping_general(data_in, "Deaths")

# I think we should include a column giving guidance on what are high, medium and low values, and a text blob on what is interpretation of each index. Will look into it.

# placeholder 1. We also may want to let users dig deeper into heaping options, using the function check_heaping_user(), but this is not yet general enough to be worth it. So just leave a marker here.

# placeholder 2 leave space here for graduation or smoothing steps. These are not yet implemented, but this would be the place in the sequence for it. In the event of modifying data like this, we'd want to bind_rows() the new data version, with a new column indicating variant, with "original" for data_in, and "smoothed" or "graduated" for modifications. Then following methods would require specifying with variant to work with. 


# when data are prepped we can do the lifetable. In future, if data have subsets, we wrap this do work on a chunk rather than siphoning columns, and we do it inside group_by() |> reframe() to scale up. We'd need to be thoughtful about how to pass chunk-specific arguments, like Sex (presumably a column). I think that's just a tradeoff, for maximal control, just do one subset at a time. Note also in future, we might not want to insist on Deaths and Exposures as the inputs. In future, we'll want to allow nMx, nqx, lx, or ex as possibilities, but not needed for the proof of concept version. Anyway, to run, we need a data.frame to pull the columns from, and we need a bunch of user-specified parameters coming from the UI, as discussed. I've annotated below as well to give hints.
data_out <- 
lt_flexible(Deaths     = data_in$Deaths,    # required
            Exposures  = data_in$Exposures, # required
            Age        = data_in$Age,       # required
            # recall all of these are passed in from the app, which will contain
            # its own default values.
            OAnew      = 100,               # basic
            age_out    = "single",          # basic
            extrapFrom = 80,                # advanced
            extrapFit  = data_in$Age[data_in$Age >= 60],  # advanced
            extrapLaw  = NULL,              # advanced
            radix      = 1e+05,             # advanced
            SRB        = 1.05,              # advanced
            a0rule     = "ak",              # advanced
            axmethod   = "un",              # advanced
            Sex        = "m")               # basic

# This plots the rates, for sake of comparison, currently just one plot, not a list of plots. You can assign the output rather than doing the immediate print, if that's better for you. In this case, data_in could also be a different subset later on when we allow for smoothing and graduation.
plot_compare_rates(data_in, 
                   data_out, 
                   extrapFrom = 80)

# TODO: plot_lifetable() not yet written, but it will create a list of 
# lifetable function plots (nqx, nMx, lx, dx) that a user might want to 
# flip through.

# TODO: lt_summary() will take data_out as its input and create a data.frame of lifetable summary statistics to display. Currently proposing e0, e65, 45q15, 5q0, sd, IQR (from LifeIneq), mode 

# Future: time series interpolation. This will probably be the gateway to bulk processing via group_by() |> reframe() for certain operations, and I don't know how to handle plotting once this is implemented. Include new plot types? Only produce plots on demand for particular years? TBD.

# Future: user guides on App usage. JC: how should this info be shared w users? Say for example the parameter extrapFrom: do we want a hover box to tell the user what it means, or do we want a "?" in a circle next to it, which opens an explanation when clicked on?

