#!/usr/bin/env Rscript
suppressPackageStartupMessages(library(survival))
args <- commandArgs(trailingOnly=TRUE)
if (length(args)!=2) stop('Usage: fit_rq2_review_merge.R INPUT.csv OUTPUT_DIR')
d <- read.csv(args[[1]],stringsAsFactors=FALSE)
out <- args[[2]]
needed <- c('pr_key','repository','agentic','merged','formal_review','inline_review_comment',
            'log1p_files_changed','test_file_presence','log1p_description_word_count')
if (!all(needed %in% names(d)) || anyNA(d[needed]) || anyDuplicated(d$pr_key)) stop('Invalid input')
for (name in c('agentic','merged','formal_review','inline_review_comment','test_file_presence')) {
  if (!all(d[[name]] %in% 0:1)) stop('Nonbinary ',name)
}
if (any(d$inline_review_comment>d$formal_review)) stop('Inline exceeds formal')
support <- tapply(d$merged,d$repository,function(v) length(unique(v))==2L)
x <- d[d$repository %in% names(support)[support],,drop=FALSE]
x$files_c <- x$log1p_files_changed-mean(x$log1p_files_changed)
x$description_c <- x$log1p_description_word_count-mean(x$log1p_description_word_count)
x$repository <- factor(x$repository)
base <- 'agentic + files_c + agentic:files_c + test_file_presence + agentic:test_file_presence + description_c + agentic:description_c'
fit_signal <- function(signal,prefix) {
  full <- as.formula(paste('merged ~',base,'+',signal,'+ agentic:',signal,'+ strata(repository)'))
  reduced <- as.formula(paste('merged ~',base,'+',signal,'+ strata(repository)'))
  fit <- clogit(full,data=x,method='efron'); null <- clogit(reduced,data=x,method='efron')
  cf <- coef(fit); vc <- vcov(fit); inter <- paste0('agentic:',signal)
  if (!(inter %in% names(cf))) inter <- paste0(signal,':agentic')
  lin <- function(w) {
    b <- sum(w*cf[names(w)]); s <- sqrt(as.numeric(t(w)%*%vc[names(w),names(w),drop=FALSE]%*%w))
    c(log_odds=b,se=s,odds_ratio=exp(b),low=exp(b-qnorm(.975)*s),high=exp(b+qnorm(.975)*s))
  }
  result <- rbind(human=lin(setNames(1,signal)),agentic=lin(setNames(c(1,1),c(signal,inter))),
                  agentic_vs_human_ratio=lin(setNames(1,inter)))
  write.csv(data.frame(contrast=rownames(result),result,row.names=NULL),file.path(out,paste0(prefix,'_contrasts.csv')),row.names=FALSE)
  tab <- as.data.frame(summary(fit)$coefficients); tab$term <- rownames(tab); rownames(tab) <- NULL
  write.csv(tab,file.path(out,paste0(prefix,'_coefficients.csv')),row.names=FALSE)
  lrt <- 2*(as.numeric(logLik(fit))-as.numeric(logLik(null)))
  write.csv(data.frame(signal=signal,n=nrow(x),repositories=nlevels(x$repository),likelihood_ratio=max(0,lrt),
                       degrees_freedom=1,p_value=pchisq(max(0,lrt),df=1,lower.tail=FALSE)),
            file.path(out,paste0(prefix,'_interaction_test.csv')),row.names=FALSE)
}
fit_signal('formal_review','formal_review')
fit_signal('inline_review_comment','inline_review')
full_joint <- as.formula(paste('merged ~',base,'+ formal_review + agentic:formal_review + inline_review_comment + agentic:inline_review_comment + strata(repository)'))
reduced_joint <- update(full_joint,. ~ . - agentic:inline_review_comment)
fit <- clogit(full_joint,data=x,method='efron'); null <- clogit(reduced_joint,data=x,method='efron')
cf <- coef(fit); vc <- vcov(fit)
lin <- function(w) {
  b <- sum(w*cf[names(w)]); s <- sqrt(as.numeric(t(w)%*%vc[names(w),names(w),drop=FALSE]%*%w))
  c(log_odds=b,se=s,odds_ratio=exp(b),low=exp(b-qnorm(.975)*s),high=exp(b+qnorm(.975)*s))
}
joint <- rbind(
  human_formal_without_inline=lin(c(formal_review=1)),
  agentic_formal_without_inline=lin(c(formal_review=1,'agentic:formal_review'=1)),
  human_inline_given_formal=lin(c(inline_review_comment=1)),
  agentic_inline_given_formal=lin(c(inline_review_comment=1,'agentic:inline_review_comment'=1)),
  agentic_vs_human_incremental_inline=lin(c('agentic:inline_review_comment'=1)))
write.csv(data.frame(contrast=rownames(joint),joint,row.names=NULL),file.path(out,'joint_contrasts.csv'),row.names=FALSE)
tab <- as.data.frame(summary(fit)$coefficients); tab$term <- rownames(tab); rownames(tab) <- NULL
write.csv(tab,file.path(out,'joint_coefficients.csv'),row.names=FALSE)
lrt <- 2*(as.numeric(logLik(fit))-as.numeric(logLik(null)))
write.csv(data.frame(n=nrow(x),repositories=nlevels(x$repository),likelihood_ratio=max(0,lrt),degrees_freedom=1,
                     p_value=pchisq(max(0,lrt),df=1,lower.tail=FALSE)),
          file.path(out,'joint_incremental_inline_test.csv'),row.names=FALSE)
write.csv(data.frame(input_rows=nrow(d),model_rows=nrow(x),repositories=nlevels(x$repository),
                     dropped_no_outcome_variation=sum(!support),dropped_repositories=paste(names(support)[!support],collapse='|'),
                     files_log1p_center=mean(x$log1p_files_changed),description_log1p_center=mean(x$log1p_description_word_count)),
          file.path(out,'population_and_centering.csv'),row.names=FALSE)
writeLines(c(capture.output(sessionInfo()),paste('formal_formula=',paste(deparse(as.formula(paste('merged ~',base,'+ formal_review + agentic:formal_review + strata(repository)'))),collapse=' ')),
             paste('joint_formula=',paste(deparse(full_joint),collapse=' ')),'method=efron',
             'interpretation=retrospective association; no historical approval, sufficiency, or causal effect'),
           file.path(out,'session_and_specification.txt'))
cat('RQ2_REVIEW_MODELS_COMPLETE\n')
