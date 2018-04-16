# Getting help with `driftR`

Thanks for using `driftR`. Before filing an issue, there are a few places
to explore and pieces to put together to make the process as smooth as possible.

Start by making a minimal reproducible example using the
[reprex](http://reprex.tidyverse.org/) package:

```r
install.packages("reprex")
library("reprex")
```

After loading the `reprex` package, copy some code to your clipboard that
includes the `library()` functions, the process you used (ideally simplified),
and the functions that are creating issues. Once the material is copied, you
can use `reprex()` to format your example:

```r
reprex()
```

For additional reprex pointers, check out the [Get help!](https://www.tidyverse.org/help/)
section of the tidyverse site.

Armed with your reprex, the next step is to figure out where to ask:

  * If it's a question about `R`: start with [community.rstudio.com](https://community.rstudio.com/).
    There are more people there to answer questions.  
  * If it's a bug: you're in the right place, file an issue.  
  * If you're not sure: let the community help you figure it out! If your
    problem _is_ a bug or a feature request, you can easily return here and
    report it.

Before opening a new issue, be sure to [search issues and pull requests](https://github.com/shaughnessyar/driftR/issues)
to make sure the bug hasn't been reported and/or already fixed in the development version. By
default, the search will be pre-populated with `is:issue is:open`. You can
[edit the qualifiers](https://help.github.com/articles/searching-issues-and-pull-requests/)
(e.g. `is:pr`, `is:closed`) as needed. For example, you'd simply
remove `is:open` to search _all_ issues in the repo, open or closed.

If you _are_ in the right place, and need to file an issue, please review the
[contributing guidelines](CONTRIBUTING.md) for this lesson.

Thanks for your help!

## Credits
This document is adopted from @jennybc's [Contribution document](https://github.com/r-lib/usethis/blob/master/.github/CONTRIBUTING.md)
for the [`usethis` package](https://github.com/r-lib/usethis).
