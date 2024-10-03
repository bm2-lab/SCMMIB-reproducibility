make_multiome_method_robustness_md <- function(metrics, metrics_name) {

    full_src <- list()

    for (method in unique(metrics$method)) {
        full_src <- c(
            full_src,
            glue::glue(
                "## {method} {{.unnumbered .tabset .tabset-pills ",
                ".tabset-fade}}"
            ),
            ""
        )

        method_metrics = metrics[metrics$method == method,]
        method_metrics_name = intersect(metrics_name, names(method_metrics)[colSums(!is.na(method_metrics)) > 0])

        full_src <- c(
            full_src,
            lapply(method_metrics_name, function(score) {
                src <- c(
                    "### <<score>> {.unnumbered}",
                    "```{r metrics-batch-<<method>>-<<score>>}",
                    "ronustness_method_multiome_heatmap(metrics, '<<method>>', '<<score>>')",
                    "```",
                    ""
                )
                knitr::knit_expand(text = src, delim = c("<<", ">>"))
            })
        )
    }

    return(full_src)
}


make_citeseq_method_robustness_md <- function(metrics, metrics_name) {

    full_src <- list()

    for (method in unique(metrics$method)) {
        full_src <- c(
            full_src,
            glue::glue(
                "## {method} {{.unnumbered .tabset .tabset-pills ",
                ".tabset-fade}}"
            ),
            ""
        )

        method_metrics = metrics[metrics$method == method,]
        method_metrics_name = intersect(metrics_name, names(method_metrics)[colSums(!is.na(method_metrics)) > 0])

        full_src <- c(
            full_src,
            lapply(method_metrics_name, function(score) {
                src <- c(
                    "### <<score>> {.unnumbered}",
                    "```{r metrics-batch-<<method>>-<<score>>}",
                    "ronustness_method_citeseq_heatmap(metrics, '<<method>>', '<<score>>')",
                    "```",
                    ""
                )
                knitr::knit_expand(text = src, delim = c("<<", ">>"))
            })
        )
    }

    return(full_src)
}


make_mosaic_method_robustness_md <- function(metrics, metrics_name, sample_1, sample_2) {

    full_src <- list()

    for (method in unique(metrics$method)) {
        full_src <- c(
            full_src,
            glue::glue(
                "## {method} {{.unnumbered .tabset .tabset-pills ",
                ".tabset-fade}}"
            ),
            ""
        )

        method_metrics = metrics[metrics$method == method,]
        method_metrics_name = intersect(metrics_name, names(method_metrics)[colSums(!is.na(method_metrics)) > 0])

        full_src <- c(
            full_src,
            lapply(method_metrics_name, function(score) {
                src <- c(
                    "### <<score>> {.unnumbered}",
                    "```{r metrics-<<method>>-<<score>>}",
                    "ronustness_method_mosaic_heatmap(metrics, '<<method>>', '<<score>>', '<<sample_1>>', '<<sample_2>>')",
                    "```",
                    ""
                )
                knitr::knit_expand(text = src, delim = c("<<", ">>"))
            })
        )
    }

    return(full_src)
}

make_mosaic_NoADT_method_robustness_md <- function(metrics, metrics_name, sample_1, sample_2) {

    full_src <- list()

    for (method in unique(metrics$method)) {
        full_src <- c(
            full_src,
            glue::glue(
                "## {method} {{.unnumbered .tabset .tabset-pills ",
                ".tabset-fade}}"
            ),
            ""
        )

        method_metrics = metrics[metrics$method == method,]
        method_metrics_name = intersect(metrics_name, names(method_metrics)[colSums(!is.na(method_metrics)) > 0])

        full_src <- c(
            full_src,
            lapply(method_metrics_name, function(score) {
                src <- c(
                    "### <<score>> {.unnumbered}",
                    "```{r metrics-NoADT-<<method>>-<<score>>}",
                    "ronustness_method_mosaic_heatmap(metrics, '<<method>>', '<<score>>', '<<sample_1>>', '<<sample_2>>')",
                    "```",
                    ""
                )
                knitr::knit_expand(text = src, delim = c("<<", ">>"))
            })
        )
    }

    return(full_src)
}


make_mosaic_cnk_method_robustness_md <- function(metrics, metrics_name, sample_1, sample_2) {

    full_src <- list()

    for (method in unique(metrics$method)) {
        full_src <- c(
            full_src,
            glue::glue(
                "## {method} {{.unnumbered .tabset .tabset-pills ",
                ".tabset-fade}}"
            ),
            ""
        )

        method_metrics = metrics[metrics$method == method,]
        method_metrics_name = intersect(metrics_name, names(method_metrics)[colSums(!is.na(method_metrics)) > 0])

        full_src <- c(
            full_src,
            lapply(method_metrics_name, function(score) {
                src <- c(
                    "### <<score>> {.unnumbered}",
                    "```{r metrics-batch-<<method>>-<<score>>}",
                    "ronustness_method_mosaic_cnk_heatmap(metrics, '<<method>>', '<<score>>', '<<sample_1>>', '<<sample_2>>')",
                    "```",
                    ""
                )
                knitr::knit_expand(text = src, delim = c("<<", ">>"))
            })
        )
    }

    return(full_src)
}

