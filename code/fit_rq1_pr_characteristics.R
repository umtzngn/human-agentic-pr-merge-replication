#!/usr/bin/env Rscript
suppressPackageStartupMessages(library(survival))
args <- commandArgs(trailingOnly=TRUE)
if (length(args)!=2) stop('Usage: fit_rq1_model_a.R INPUT.csv OUTPUT_DIR')
d <- read.csv(args[[1]],stringsAsFactors=FALSE,check.names=FALSE)
out <- args[[2]]
bool <- function(x) {
  y <- tolower(as.character(x)); if (!all(y %in% c('true','false','1','0'))) stop('Invalid Boolean')
  y %in% c('true','1')
}
needed <- c('pr_key','repository','source','resolved','merged_resolved_only',
            'candidate_spec_a_complete_case','log1p_files_changed',
            'test_file_presence','log1p_description_word_count')
if (!all(needed %in% names(d)) || anyDuplicated(d$pr_key)) stop('Missing or duplicate input')
d$resolved <- bool(d$resolved)
d$candidate_spec_a_complete_case <- bool(d$candidate_spec_a_complete_case)
d$test_file_presence <- as.integer(bool(d$test_file_presence))
d$merged_resolved_only <- as.integer(d$merged_resolved_only)
x <- d[d$resolved & d$candidate_spec_a_complete_case & d$merged_resolved_only %in% 0:1,,drop=FALSE]
if (!all(x$source %in% c('human','agentic')) || anyNA(x[needed])) stop('Invalid Model A rows')
support <- tapply(x$merged_resolved_only,x$repository,function(v) length(unique(v))==2L)
dropped <- names(support)[!support]
x <- x[x$repository %in% names(support)[support],,drop=FALSE]
x$source <- factor(x$source,levels=c('human','agentic'))
x$repository <- factor(x$repository)
files_center <- mean(x$log1p_files_changed)
description_center <- mean(x$log1p_description_word_count)
x$files_c <- x$log1p_files_changed-files_center
x$description_c <- x$log1p_description_word_count-description_center
full <- merged_resolved_only ~ source + files_c + test_file_presence + description_c +
  source:files_c + source:test_file_presence + source:description_c + strata(repository)
reduced <- merged_resolved_only ~ source + files_c + test_file_presence + description_c + strata(repository)
fit <- clogit(full,data=x,method='efron')
null <- clogit(reduced,data=x,method='efron')
cf <- coef(fit); vc <- vcov(fit)
if (!all(is.finite(cf)) || !all(is.finite(vc))) stop('Nonfinite Model A result')
tab <- as.data.frame(summary(fit)$coefficients); tab$term <- rownames(tab); rownames(tab) <- NULL
names(tab)[1:5] <- c('estimate_log_odds','odds_ratio','standard_error','z','p_value_raw')
interaction <- grepl('sourceagentic:',tab$term,fixed=TRUE) | grepl(':sourceagentic',tab$term,fixed=TRUE)
tab$p_value_holm_interactions <- NA_real_
tab$p_value_holm_interactions[interaction] <- p.adjust(tab$p_value_raw[interaction],method='holm')
write.csv(tab,file.path(out,'coefficients.csv'),row.names=FALSE)
lrt <- 2*(as.numeric(logLik(fit))-as.numeric(logLik(null)))
write.csv(data.frame(n=nrow(x),repositories=nlevels(x$repository),likelihood_ratio=max(0,lrt),
                     degrees_freedom=3,p_value=pchisq(max(0,lrt),df=3,lower.tail=FALSE)),
          file.path(out,'joint_interaction_test.csv'),row.names=FALSE)
contrast <- function(main,interaction) {
  human_b <- unname(cf[[main]])
  human_s <- sqrt(unname(vc[main,main]))
  agentic_b <- unname(cf[[main]]+cf[[interaction]])
  agentic_s <- sqrt(unname(vc[main,main]+vc[interaction,interaction]+2*vc[main,interaction]))
  b <- c(human_b,agentic_b)
  s <- c(human_s,agentic_s)
  data.frame(
    source=c('human','agentic'),
    log_odds=b,
    standard_error=s,
    odds_ratio=exp(b),
    confidence_low=exp(b-qnorm(.975)*s),
    confidence_high=exp(b+qnorm(.975)*s),
    stringsAsFactors=FALSE
  )
}
items <- list(files_changed=c('files_c','sourceagentic:files_c'),
              test_file_presence=c('test_file_presence','sourceagentic:test_file_presence'),
              description_length=c('description_c','sourceagentic:description_c'))
rows <- do.call(rbind,lapply(names(items),function(name) {
  z <- contrast(items[[name]][1],items[[name]][2])
  data.frame(characteristic=name,z,row.names=NULL,check.names=FALSE)
}))
write.csv(rows,file.path(out,'source_specific_associations.csv'),row.names=FALSE)
write.csv(data.frame(input_rows=nrow(d),model_rows=nrow(x),repositories=nlevels(x$repository),
                     dropped_no_outcome_variation=length(dropped),dropped_repositories=paste(dropped,collapse='|'),
                     files_log1p_center=files_center,description_log1p_center=description_center),
          file.path(out,'population_and_centering.csv'),row.names=FALSE)
writeLines(c(capture.output(sessionInfo()),paste('full_formula=',paste(deparse(full),collapse=' ')),
             paste('reduced_formula=',paste(deparse(reduced),collapse=' ')),'method=efron'),
           file.path(out,'session_and_specification.txt'))
cat('RQ1_MODEL_A_COMPLETE\n')
