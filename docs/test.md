Update 2019-10-07
=================

On the sample space
-------------------

On a space of summary statistics
--------------------------------

    library(igraph)
    library(purrr)
    library(digest)
    library(StartNetwork)

    # Barabasi Albert model

    x <- replicate(1000, sample_pa(8, directed = FALSE), simplify = FALSE)

    plot(x[[1]])

![](test_files/figure-markdown_strict/unnamed-chunk-2-1.png)

    y <- sapply(x, purrr::compose(digest, igraph::as_adj))

    entropy_calc(y)

    ## [1] 8.247243
