metric_method_barplot <- function(metrics, score, palette){

    library(forcats)

    df_summary <- metrics %>%
        group_by(method) %>%
        summarise(mean_score = mean(!!sym(score)),
                  min_score = min(!!sym(score)),
                  max_score = max(!!sym(score)))
    overall_median <- median(metrics[,score], na.rm = TRUE)

    plot <- ggplot(df_summary, aes(x = fct_rev(method), y = mean_score, fill = method)) +
        geom_bar(stat = "identity", width = 0.7) +
        geom_errorbar(aes(ymin = min_score, ymax = max_score), width = 0.2, color = "black") +
        geom_hline(yintercept = overall_median, linetype = "dashed", color = "red", linewidth = 1) +
        scale_fill_manual(values = palette, drop = FALSE, name = "Method") +
        labs(x = "Method", y = score) +
        ylim(0, 1) +
        coord_flip() +
        theme_minimal() +
        theme(panel.border = element_rect(color = "black", fill = NA, size = 1))

    plot
}


imp_metric_method_lineplot <- function(metrics, score, palette){

    metric <- metrics[, c("method", score, "RNA", "ATAC")]

    plot <- ggplot(metric, aes(x=RNA, y=!!sym(score), colour=method)) +
        geom_line() +
        geom_point() +
        ylim(0, 1) +
        scale_color_manual(values = methods_pallette, drop = FALSE, name = "Method") +
        xlab("gradient of downsampling") +
        theme_minimal() +
        theme(panel.border = element_rect(color = "black", fill = NA, size = 1))

    plot
}


ronustness_method_multiome_heatmap <- function(metrics, method, score){

    suppressMessages(library(ggplot2))
    suppressMessages(library(viridis))

    metric = metrics[which(metrics$method == method),c("method", score, "RNA", "ATAC")]
    metric$RNA <- factor(metric$RNA, levels = c("10", "25", "50", "75", "100"))
    metric$ATAC <- factor(metric$ATAC, levels = c("100", "75", "50", "25", "10"))

    plot <- ggplot(metric, aes(x = RNA, y = ATAC, fill = !!sym(score))) +
        geom_tile() +
        geom_text(aes(label = !!sym(score)), color = "red", size = 4) +
        scale_fill_viridis(name = "Score", option = "D") +
        labs(x = "RNA", y = "ATAC") +
        theme_minimal() +
        theme(
            panel.grid = element_blank(),
            panel.border = element_rect(color = "black", fill = NA, size = 1)
        )
    plot
}


ronustness_method_citeseq_heatmap <- function(metrics, method, score){

    suppressMessages(library(ggplot2))
    suppressMessages(library(viridis))

    metric = metrics[which(metrics$method == method),c("method", score, "RNA", "ADT")]
    metric$RNA <- factor(metric$RNA, levels = c("10", "25", "50", "75", "100"))
    metric$ADT <- factor(metric$ADT, levels = c("100", "75", "50", "25", "10"))

    plot <- ggplot(metric, aes(x = RNA, y = ADT, fill = !!sym(score))) +
        geom_tile() +
        geom_text(aes(label = !!sym(score)), color = "red", size = 4) +
        scale_fill_viridis(name = "Score", option = "D") +
        labs(x = "RNA", y = "ADT") +
        theme_minimal() +
        theme(
            panel.grid = element_blank(),
            panel.border = element_rect(color = "black", fill = NA, size = 1)
        )
    plot
}


ronustness_method_mosaic_heatmap <- function(metrics, method, score, sample_1, sample_2){

    suppressMessages(library(ggplot2))
    suppressMessages(library(viridis))

    metric = metrics[which(metrics$method == method),c("method", score, sample_1, sample_2)]
    metric[, sample_1] <- factor(metric[, sample_1], levels = c("10", "25", "50", "75", "100"))
    metric[, sample_2] <- factor(metric[, sample_2], levels = c("100", "75", "50", "25", "10"))

    plot <- ggplot(metric, aes(x = !!sym(sample_1), y = !!sym(sample_2), fill = !!sym(score))) +
        geom_tile() +
        geom_text(aes(label = !!sym(score)), color = "red", size = 4) +
        scale_fill_viridis(name = "Score", option = "D") +
        labs(x = sample_1, y = sample_2) +
        theme_minimal() +
        theme(
            panel.grid = element_blank(),
            panel.border = element_rect(color = "black", fill = NA, size = 1)
        )
    plot
}


ronustness_method_mosaic_cnk_heatmap <- function(metrics, method, score, sample_1, sample_2){

    suppressMessages(library(ggplot2))
    suppressMessages(library(viridis))

    metric = metrics[which(metrics$method == method),c("method", score, sample_1, sample_2)]
    metric[, sample_1] <- factor(metric[, sample_1], levels = c("3000", "5000", "10000", "20000"))

    plot <- ggplot(metric, aes(x = !!sym(sample_1), y = !!sym(sample_2), fill = !!sym(score))) +
        geom_tile() +
        geom_text(aes(label = !!sym(score)), color = "red", size = 4) +
        scale_fill_viridis(name = "Score", option = "D") +
        labs(x = sample_1, y = sample_2) +
        coord_fixed(ratio = 1) +
        theme_minimal() +
        theme(
            panel.grid = element_blank(),
            panel.border = element_rect(color = "black", fill = NA, size = 1)
        )
    plot
}

