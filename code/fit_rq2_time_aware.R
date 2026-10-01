#!/usr/bin/env Rscript
suppressPackageStartupMessages(library(survival))
args <- commandArgs(trailingOnly=TRUE)
if (length(args)!=2) stop('Usage: fit_time_aware.R INPUT.csv.gz OUTPUT_DIR')
d <- read.csv(gzfile(args[[1]]),stringsAsFactors=FALSE); out <- args[[2]]
needed <- c('pr_key','repository','agentic','log1p_files_changed','test_file_presence',
            'log1p_description_word_count','tstart_seconds','tstop_seconds','formal_t','inline_t','terminal_event')
if (!all(needed %in% names(d)) || anyNA(d[needed])) stop('Missing interval data')
if (!all(d$agentic %in% 0:1) || !all(d$formal_t %in% 0:1) || !all(d$inline_t %in% 0:1) ||
    !all(d$test_file_presence %in% 0:1) || !all(d$terminal_event %in% 0:2) || any(d$inline_t>d$formal_t)) stop('Invalid indicator')
first <- !duplicated(d$pr_key)
d$files_c <- d$log1p_files_changed-mean(d$log1p_files_changed[first])
d$description_c <- d$log1p_description_word_count-mean(d$log1p_description_word_count[first])
d$repository <- factor(d$repository)
formula <- Surv(tstart_seconds,tstop_seconds,event) ~ agentic + formal_t + agentic:formal_t +
  inline_t + agentic:inline_t + files_c + agentic:files_c + test_file_presence + agentic:test_file_presence +
  description_c + agentic:description_c + strata(repository) + cluster(pr_key)
fit_one <- function(code,label) {
  d$event <- as.integer(d$terminal_event==code)
  fit <- coxph(formula,data=d,ties='efron',x=TRUE,model=TRUE)
  cf <- coef(fit); vc <- vcov(fit)
  lin <- function(w) {
    b <- sum(w*cf[names(w)]); s <- sqrt(as.numeric(t(w)%*%vc[names(w),names(w),drop=FALSE]%*%w))
    c(log_hazard=b,robust_se=s,hazard_ratio=exp(b),low=exp(b-qnorm(.975)*s),high=exp(b+qnorm(.975)*s),
      robust_wald_p=pchisq((b/s)^2,df=1,lower.tail=FALSE))
  }
  result <- rbind(human_formal_when_no_inline=lin(c(formal_t=1)),
                  agentic_formal_when_no_inline=lin(c(formal_t=1,'agentic:formal_t'=1)),
                  human_inline_given_formal=lin(c(inline_t=1)),
                  agentic_inline_given_formal=lin(c(inline_t=1,'agentic:inline_t'=1)),
                  agentic_vs_human_inline_ratio=lin(c('agentic:inline_t'=1)),
                  agentic_vs_human_formal_ratio=lin(c('agentic:formal_t'=1)))
  write.csv(data.frame(outcome=label,contrast=rownames(result),result,row.names=NULL),file.path(out,paste0(label,'_contrasts.csv')),row.names=FALSE)
  coefs <- data.frame(term=names(cf),estimate=unname(cf),robust_se=sqrt(diag(vc)),row.names=NULL)
  write.csv(coefs,file.path(out,paste0(label,'_coefficients.csv')),row.names=FALSE)
  ph <- as.data.frame(cox.zph(fit)$table); ph$term <- rownames(ph)
  write.csv(ph,file.path(out,paste0(label,'_ph_diagnostic.csv')),row.names=FALSE)
  data.frame(outcome=label,events=sum(d$event),log_partial_likelihood=tail(fit$loglik,1))
}
summary <- rbind(fit_one(1,'merge_rate'),fit_one(2,'unmerged_closure_rate'))
write.csv(summary,file.path(out,'fit_summary.csv'),row.names=FALSE)
writeLines(c(capture.output(sessionInfo()),paste('formula=',paste(deparse(formula),collapse=' ')),
             paste('subjects=',length(unique(d$pr_key))),paste('intervals=',nrow(d)),
             'ties=efron','open_at_cutoff_right_censored=TRUE','cluster_robust_se_by_pr=TRUE',
             'interpretation=cause-specific event-rate association while recorded PR remains open; not eventual merge probability or causal effect'),
           file.path(out,'session_and_specification.txt'))
cat('RQ2_TIME_AWARE_COMPLETE\n')
