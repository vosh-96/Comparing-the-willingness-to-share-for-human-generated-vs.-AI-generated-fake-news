function (data, x, y, type = "parametric", pairwise.display = "significant", 
          p.adjust.method = "holm", effsize.type = "unbiased", bf.prior = 0.707, 
          bf.message = TRUE, results.subtitle = TRUE, xlab = NULL, 
          ylab = NULL, caption = NULL, title = NULL, subtitle = NULL, 
          k = 2L, conf.level = 0.95, nboot = 100L, tr = 0.2, centrality.plotting = TRUE, 
          centrality.type = type, centrality.point.args = list(size = 5, 
                                                               color = "darkred"), centrality.label.args = list(size = 3, 
                                                                                                                nudge_x = 0.4, segment.linetype = 4), centrality.path = TRUE, 
          centrality.path.args = list(linewidth = 1, color = "red", 
                                      alpha = 0.5), point.args = list(size = 3, alpha = 0.5, 
                                                                      na.rm = TRUE), point.path = TRUE, point.path.args = list(alpha = 0.5, 
                                                                                                                               linetype = "dashed"), boxplot.args = list(width = 0.2, 
                                                                                                                                                                         alpha = 0.5, na.rm = TRUE), violin.args = list(width = 0.5, 
                                                                                                                                                                                                                        alpha = 0.2, na.rm = TRUE), ggsignif.args = list(textsize = 3, 
                                                                                                                                                                                                                                                                         tip_length = 0.01, na.rm = TRUE), ggtheme = ggstatsplot::theme_ggstatsplot(), 
          package = "RColorBrewer", palette = "Dark2", ggplot.component = NULL, 
          ...) 
{
  c(x, y) %<-% c(ensym(x), ensym(y))
  type <- stats_type_switch(type)
  data %<>% select({
    {
      x
    }
  }, {
    {
      y
    }
  }) %>% mutate(`:=`({
    {
      x
    }
  }, droplevels(as.factor({
    {
      x
    }
  })))) %>% mutate(.rowid = row_number(), .by = {
    {
      x
    }
  }) %>% anti_join(x = ., y = filter(., is.na({
    {
      y
    }
  })), by = ".rowid")
  test <- ifelse(nlevels(data %>% pull({
    {
      x
    }
  })) < 3L, "t", "anova")
  if (results.subtitle && insight::check_if_installed("afex")) {
    .f.args <- list(data = data, x = as_string(x), y = as_string(y), 
                    effsize.type = effsize.type, conf.level = conf.level, 
                    k = k, tr = tr, paired = TRUE, bf.prior = bf.prior, 
                    nboot = nboot)
    .f <- .f_switch(test)
    subtitle_df <- .eval_f(.f, !!!.f.args, type = type)
    subtitle <- if (!is.null(subtitle_df)) 
      subtitle_df$expression[[1L]]
    if (type == "parametric" && bf.message) {
      caption_df <- .eval_f(.f, !!!.f.args, type = "bayes")
      caption <- if (!is.null(caption_df)) 
        caption_df$expression[[1L]]
    }
  }
  plot <- ggplot(data, aes({
    {
      x
    }
  }, {
    {
      y
    }
  }, group = .rowid)) + exec(geom_point, aes(color = {
    {
      x
    }
  }), !!!point.args) + exec(geom_boxplot, aes({
    {
      x
    }
  }, {
    {
      y
    }
  }), inherit.aes = FALSE, !!!boxplot.args, outlier.shape = NA) + 
    exec(geom_violin, aes(x = {
      {
        x
      }
    }, y = {
      {
        y
      }
    }, fill = {
      {
        x
      }
    }), inherit.aes = FALSE, !!!violin.args)
  if (test == "t" && point.path) 
    plot <- plot + exec(geom_path, !!!point.path.args)
  if (isTRUE(centrality.plotting)) {
    plot <- suppressWarnings(.centrality_ggrepel(plot = plot, 
                                                 data = data, x = {
                                                   {
                                                     x
                                                   }
                                                 }, y = {
                                                   {
                                                     y
                                                   }
                                                 }, k = k, type = stats_type_switch(centrality.type), 
                                                 tr = tr, centrality.path = centrality.path, centrality.path.args = centrality.path.args, 
                                                 centrality.point.args = centrality.point.args, centrality.label.args = centrality.label.args))
  }
  seclabel <- NULL
  if (pairwise.display != "none" && test == "anova") {
    mpc_df <- pairwise_comparisons(data = data, x = {
      {
        x
      }
    }, y = {
      {
        y
      }
    }, type = type, tr = tr, paired = TRUE, p.adjust.method = p.adjust.method, 
    k = k)
    plot <- .ggsignif_adder(plot = plot, mpc_df = mpc_df, 
                            data = data, x = {
                              {
                                x
                              }
                            }, y = {
                              {
                                y
                              }
                            }, pairwise.display = pairwise.display, ggsignif.args = ggsignif.args)
    seclabel <- .pairwise_seclabel(unique(mpc_df$test), ifelse(type == 
                                                                 "bayes", "all", pairwise.display))
  }
  .aesthetic_addon(plot = plot, x = data %>% pull({
    {
      x
    }
  }), xlab = xlab %||% as_name(x), ylab = ylab %||% as_name(y), 
  title = title, subtitle = subtitle, caption = caption, 
  seclabel = seclabel, ggtheme = ggtheme, package = package, 
  palette = palette, ggplot.component = ggplot.component)
}
