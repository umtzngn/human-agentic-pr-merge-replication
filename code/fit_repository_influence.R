#!/usr/bin/env Rscript
suppressPackageStartupMessages(library(survival))
args <- commandArgs(trailingOnly=TRUE)
if (length(args)!=3) stop('Usage: fit_repository_influence.R RQ1.csv RQ2.csv OUTPUT_DIR')
rq1 <- read.csv(args[[1]],stringsAsFactors=FALSE); rq2 <- read.csv(args[[2]],stringsAsFactors=FALSE); out <- args[[3]]
prepare <- function(d) {
  d$repository <- factor(d$repository); d$source <- factor(d$source,levels=c('human','agentic'))
  d$merged <- as.integer(d$merged); d$test_file_presence <- as.integer(d$test_file_presence)
  d$files_c <- d$log1p_files_changed-mean(d$log1p_files_changed)
  d$description_c <- d$log1p_description_word_count-mean(d$log1p_description_word_count)
  d
}
rq1 <- prepare(rq1); rq2 <- prepare(rq2)
sizes <- sort(table(rq2$repository),decreasing=TRUE)
top <- names(sizes)[seq_len(min(10,length(sizes)))]
write.csv(data.frame(repository=top,resolved_prs=as.integer(sizes[top]),rank=seq_along(top)),file.path(out,'top_repositories.csv'),row.names=FALSE)
term <- function(cf,parts) {
  found <- names(cf)[sapply(strsplit(names(cf),':',fixed=TRUE),function(x) length(x)==length(parts) && setequal(x,parts))]
  if (length(found)!=1) stop('Term not found: ',paste(parts,collapse=':'))
  found[[1]]
}
lrt <- function(full,reduced) {
  value <- max(0,2*(as.numeric(logLik(full))-as.numeric(logLik(reduced))))
  df <- attr(logLik(full),'df')-attr(logLik(reduced),'df')
  c(chisq=value,df=df,p_value=pchisq(value,df=df,lower.tail=FALSE))
}
rq1_one <- function(exclude) {
  x <- if (is.na(exclude)) rq1 else rq1[rq1$repository!=exclude,,drop=FALSE]
  x$repository <- droplevels(x$repository)
  fullf <- merged ~ source + files_c + test_file_presence + description_c + source:files_c + source:test_file_presence + source:description_c + strata(repository)
  reducedf <- merged ~ source + files_c + test_file_presence + description_c + strata(repository)
  fit <- clogit(fullf,data=x,method='efron'); null <- clogit(reducedf,data=x,method='efron'); cf <- coef(fit); vc <- vcov(fit)
  interactions <- c(files_changed=term(cf,c('sourceagentic','files_c')),
                    test_file_presence=term(cf,c('sourceagentic','test_file_presence')),
                    description_length=term(cf,c('sourceagentic','description_c')))
  rows <- do.call(rbind,lapply(names(interactions),function(name) {
    key <- interactions[[name]]; b <- cf[[key]]; s <- sqrt(vc[key,key])
    data.frame(excluded_repository=ifelse(is.na(exclude),'NONE',exclude),characteristic=name,
               interaction_log_odds=b,standard_error=s,interaction_odds_ratio=exp(b),
               confidence_low=exp(b-qnorm(.975)*s),confidence_high=exp(b+qnorm(.975)*s),wald_p=pchisq((b/s)^2,1,lower.tail=FALSE))
  }))
  test <- lrt(fit,null); rows$joint_interaction_chisq <- test[['chisq']]; rows$joint_df <- test[['df']]; rows$joint_p <- test[['p_value']]
  rows$n <- nrow(x); rows$repositories <- nlevels(x$repository); rows
}
rq2_one <- function(exclude) {
  x <- if (is.na(exclude)) rq2 else rq2[rq2$repository!=exclude,,drop=FALSE]
  x$repository <- droplevels(x$repository)
  controls <- 'agentic + files_c + agentic:files_c + test_file_presence + agentic:test_file_presence + description_c + agentic:description_c'
  formal_f <- as.formula(paste('merged ~',controls,'+ formal_review + agentic:formal_review + strata(repository)'))
  formal_n <- update(formal_f,. ~ . - agentic:formal_review)
  joint_f <- as.formula(paste('merged ~',controls,'+ formal_review + agentic:formal_review + inline_review_comment + agentic:inline_review_comment + strata(repository)'))
  joint_n <- update(joint_f,. ~ . - agentic:inline_review_comment)
  ff <- clogit(formal_f,data=x,method='efron'); fn <- clogit(formal_n,data=x,method='efron')
  jf <- clogit(joint_f,data=x,method='efron'); jn <- clogit(joint_n,data=x,method='efron')
  formal_key <- term(coef(ff),c('agentic','formal_review')); inline_key <- term(coef(jf),c('agentic','inline_review_comment'))
  make <- function(label,fit,key,test) {
    b <- coef(fit)[[key]]; s <- sqrt(vcov(fit)[key,key])
    data.frame(excluded_repository=ifelse(is.na(exclude),'NONE',exclude),signal=label,
               interaction_log_odds=b,standard_error=s,interaction_odds_ratio=exp(b),
               confidence_low=exp(b-qnorm(.975)*s),confidence_high=exp(b+qnorm(.975)*s),
               wald_p=pchisq((b/s)^2,1,lower.tail=FALSE),interaction_lrt_chisq=test[['chisq']],
               interaction_lrt_df=test[['df']],interaction_lrt_p=test[['p_value']],n=nrow(x),repositories=nlevels(x$repository))
  }
  rbind(make('formal_review_primary',ff,formal_key,lrt(ff,fn)),make('incremental_inline_given_formal',jf,inline_key,lrt(jf,jn)))
}
exclusions <- c(NA_character_,top)
rq1_out <- do.call(rbind,lapply(exclusions,rq1_one)); rq2_out <- do.call(rbind,lapply(exclusions,rq2_one))
write.csv(rq1_out,file.path(out,'rq1_influence.csv'),row.names=FALSE)
write.csv(rq2_out,file.path(out,'rq2_influence.csv'),row.names=FALSE)
writeLines(c(capture.output(sessionInfo()),'scope=full primary data plus leave-one-out checks for the ten largest repositories',
             'role=repository-influence sensitivity; not a new primary model'),file.path(out,'session_and_specification.txt'))
cat('REPOSITORY_INFLUENCE_COMPLETE\n')
