#!/usr/bin/env Rscript
suppressPackageStartupMessages(library(survival))
args <- commandArgs(trailingOnly=TRUE)
if (length(args)!=2) stop('Usage: fit_piecewise_time.R INPUT.csv.gz OUTPUT_DIR')
d <- read.csv(gzfile(args[[1]]),stringsAsFactors=FALSE,check.names=FALSE)
out <- args[[2]]
bands <- c('d00_01','d01_03','d03_07','d07_30','d30_plus')
d$time_band <- factor(d$time_band,levels=bands)
d$band_d01_03 <- as.integer(d$time_band=='d01_03')
d$band_d03_07 <- as.integer(d$time_band=='d03_07')
d$band_d07_30 <- as.integer(d$time_band=='d07_30')
d$band_d30_plus <- as.integer(d$time_band=='d30_plus')
band_variables <- c('band_d01_03','band_d03_07','band_d07_30','band_d30_plus')
d$repository <- factor(d$repository)
d$pr_key <- factor(d$pr_key)
for (name in c('agentic','formal_t','inline_t','test_file_presence','terminal_event')) d[[name]] <- as.integer(d[[name]])
if (anyNA(d) || any(d$inline_t>d$formal_t) || any(!d$terminal_event %in% 0:2)) stop('Invalid piecewise input')
first <- !duplicated(d$pr_key)
d$files_c <- d$log1p_files_changed-mean(d$log1p_files_changed[first])
d$description_c <- d$log1p_description_word_count-mean(d$log1p_description_word_count[first])
base <- c('agentic','formal_t','inline_t','files_c','test_file_presence','description_c',
          'agentic:formal_t','agentic:inline_t','agentic:files_c',
          'agentic:test_file_presence','agentic:description_c')
time_terms <- unlist(lapply(band_variables,function(band) paste0('(',base,'):',band)))
rhs <- paste(c(base,time_terms,
               'strata(repository,time_band)','cluster(pr_key)'),collapse=' + ')
formula <- as.formula(paste('Surv(tstart_seconds,tstop_seconds,event) ~',rhs))
term_name <- function(cf,parts) {
  candidates <- names(cf)[sapply(strsplit(names(cf),':',fixed=TRUE),function(x) length(x)==length(parts) && setequal(x,parts))]
  if (length(candidates)!=1) stop('Cannot identify term: ',paste(parts,collapse=':'))
  candidates[[1]]
}
linear <- function(cf,vc,weights) {
  b <- sum(weights*cf[names(weights)])
  s <- sqrt(as.numeric(t(weights)%*%vc[names(weights),names(weights),drop=FALSE]%*%weights))
  c(log_hazard=b,robust_se=s,hazard_ratio=exp(b),confidence_low=exp(b-qnorm(.975)*s),
    confidence_high=exp(b+qnorm(.975)*s),robust_wald_p=pchisq((b/s)^2,df=1,lower.tail=FALSE))
}
contrast <- function(fit,signal,agentic_value,band) {
  cf <- coef(fit); vc <- vcov(fit); names <- c(signal)
  if (agentic_value==1) names <- c(names,term_name(cf,c('agentic',signal)))
  if (band!=bands[[1]]) {
    band_term <- paste0('band_',band)
    names <- c(names,term_name(cf,c(signal,band_term)))
    if (agentic_value==1) names <- c(names,term_name(cf,c('agentic',signal,band_term)))
  }
  linear(cf,vc,setNames(rep(1,length(names)),names))
}
ratio <- function(fit,signal,band) {
  cf <- coef(fit); vc <- vcov(fit); names <- c(term_name(cf,c('agentic',signal)))
  if (band!=bands[[1]]) names <- c(names,term_name(cf,c('agentic',signal,paste0('band_',band))))
  linear(cf,vc,setNames(rep(1,length(names)),names))
}
joint_time_test <- function(fit,parts,label) {
  cf <- coef(fit); vc <- vcov(fit)
  selected <- names(cf)[sapply(strsplit(names(cf),':',fixed=TRUE),function(x) {
    length(x)==length(parts)+1 && all(parts %in% x) && sum(grepl('^band_',x))==1
  })]
  if (length(selected)!=4) stop('Unexpected time interaction count for ',label)
  b <- cf[selected]; v <- vc[selected,selected,drop=FALSE]
  stat <- as.numeric(t(b)%*%solve(v,b))
  data.frame(test=label,robust_wald_chisq=stat,degrees_freedom=length(selected),
             p_value=pchisq(stat,df=length(selected),lower.tail=FALSE))
}
fit_one <- function(code,label) {
  d$event <- as.integer(d$terminal_event==code)
  fit <- coxph(formula,data=d,ties='efron',x=TRUE,model=TRUE)
  if (anyNA(coef(fit)) || any(!is.finite(coef(fit))) || any(!is.finite(vcov(fit)))) stop('Nonfinite piecewise fit: ',label)
  rows <- do.call(rbind,lapply(bands,function(band) {
    rbind(data.frame(outcome=label,time_band=band,contrast='human_formal_when_no_inline',t(contrast(fit,'formal_t',0,band)),row.names=NULL),
          data.frame(outcome=label,time_band=band,contrast='agentic_formal_when_no_inline',t(contrast(fit,'formal_t',1,band)),row.names=NULL),
          data.frame(outcome=label,time_band=band,contrast='agentic_vs_human_formal_ratio',t(ratio(fit,'formal_t',band)),row.names=NULL),
          data.frame(outcome=label,time_band=band,contrast='human_inline_given_formal',t(contrast(fit,'inline_t',0,band)),row.names=NULL),
          data.frame(outcome=label,time_band=band,contrast='agentic_inline_given_formal',t(contrast(fit,'inline_t',1,band)),row.names=NULL),
          data.frame(outcome=label,time_band=band,contrast='agentic_vs_human_inline_ratio',t(ratio(fit,'inline_t',band)),row.names=NULL))
  }))
  write.csv(rows,file.path(out,paste0(label,'_band_contrasts.csv')),row.names=FALSE)
  tab <- data.frame(term=names(coef(fit)),estimate=unname(coef(fit)),robust_se=sqrt(diag(vcov(fit))),row.names=NULL)
  write.csv(tab,file.path(out,paste0(label,'_coefficients.csv')),row.names=FALSE)
  tests <- rbind(joint_time_test(fit,c('agentic','formal_t'),paste0(label,'_agentic_formal_difference_changes_over_time')),
                 joint_time_test(fit,c('agentic','inline_t'),paste0(label,'_agentic_inline_difference_changes_over_time')),
                 joint_time_test(fit,c('formal_t'),paste0(label,'_human_formal_association_changes_over_time')),
                 joint_time_test(fit,c('inline_t'),paste0(label,'_human_inline_association_changes_over_time')))
  write.csv(tests,file.path(out,paste0(label,'_time_variation_tests.csv')),row.names=FALSE)
  data.frame(outcome=label,events=sum(d$event),subjects=length(unique(d$pr_key)),intervals=nrow(d),coefficients=length(coef(fit)))
}
summary <- rbind(fit_one(1,'merge_rate'),fit_one(2,'unmerged_closure_rate'))
write.csv(summary,file.path(out,'fit_summary.csv'),row.names=FALSE)
writeLines(c(capture.output(sessionInfo()),paste('formula=',paste(deparse(formula),collapse=' ')),
             'bands=0-1 day|1-3 days|3-7 days|7-30 days|over 30 days',
             'repository_and_time_band_specific_baseline_hazards=TRUE','cluster_robust_se_by_pr=TRUE',
             'role=time-aware sensitivity; not the primary RQ2 estimate'),
           file.path(out,'session_and_specification.txt'))
cat('PIECEWISE_TIME_MODELS_COMPLETE\n')
