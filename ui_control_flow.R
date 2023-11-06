
# you can load with library(), if you're
# certain to have the up-to-date version
devtools::load_all()


# user_file is presumed given from the UI;
# this way of getting data into memory is optional;
# you might handle data loading more directly using
# some shiny tools. In this case, let's expect
# a data.frame called data_in, like the one we see here;

user_file <- "abridged_data.csv"

# character string of file name, presumed in data/ folder
# returns data.frame
data_in <- read_data(user_file)

# Note, column AgeInt is actually optional
# Note, column Mx_emp is currently created by
# read_data(), but it could be created on the 
# fly elsewhere when needed, we don't necessarily
# need to insist on having it now.

# This is data.frame in, data.frame out;
# the created data.frame is a table of checks, pass status, and messages for any failures. The failure messages should be displayed usefully somewhere. The specific checks are subject to changes.
check_data(data_in)

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

heaping_table <- check_heaping_general(data_in)


data_out <- 

