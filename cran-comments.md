## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Previous CRAN reviewer comments 2023-11-10

Thank you for your helpful comments. I've tried to address them and have provided comments for each:

> You write information messages to the console that cannot be easily suppressed. It is more R like to generate objects that can be used to extract the information a user is interested in, and then print() that object.
> Instead of print()/cat() rather use message()/warning()  or if(verbose)cat(..) (or maybe stop()) if you really have to write text to the console.
> (except for print, summary, interactive functions)
> e.g.: R/trace.R 

I have changed the code_tree function to return a character vector with an extra
class, and written a print function for that class. So that way the user can
choose whether the collect the output or to print it.

> \dontrun{} should only be used if the example really cannot be executed (e.g. because of missing additional software, missing API keys, ...) by the user. That's why wrapping examples in \dontrun{} adds the comment ("# Not run:") as a warning for the user. Does not seem necessary. Please unwrap the examples if they are executable in < 5 sec, or replace \dontrun{} with \donttest{}.

I had wanted to avoid the use of \dontrun{}, however, I get the following note
on some systems when running checks for CRAN on rhub
(This is taken from https://builder.r-hub.io/status/trace.package_0.1.1.tar.gz-43e6ebae32dd438e98e127234f28b696)
Examples with CPU (user + system) or elapsed time > 5s
               user system elapsed
  code_tree   6.277  0.104  19.183
  flame_graph 2.729  0.005   8.324
  run_trace   2.630  0.008   7.656
  
On my home system, the examples run quite quickly, but was just following the
advice provided.

> If there are references describing the methods in your package, please add these in the description field of your DESCRIPTION file in the form authors (year) <doi:...> authors (year) <arXiv:...> authors (year, ISBN:...) or if those are not available: [https:...]<https:...> with no space after 'doi:', 'arXiv:', 'https:' and angle brackets for auto-linking. (If you want to add a title as well please put it in quotes: "Title")

There are no references for the methods. 
