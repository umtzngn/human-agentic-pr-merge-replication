#!/usr/bin/env Rscript
suppressPackageStartupMessages(library(survival))
args <- commandArgs(trailingOnly=TRUE)
if (length(args)!=4) stop('Usage: fit_family_models.R RQ1.csv RQ2.csv MAPPING.csv OUTPUT_DIR')
rq1 <- read.csv(args[[1]],stringsAsFactors=FALSE,check.names=FALSE)
rq2 <- read.csv(args[[2]],stringsAsFactors=FALSE,check.names=FALSE)
mapping <- read.csv(args[[3]],stringsAsFactors=FALSE)
out <- args[[4]]
levels_group <- mapping$group_id[order(mapping$group_order)]
if (levels_group[[1]]!='human' || length(levels_group)!=7) stop('Unexpected family mapping')

prepare <- function(d) {
  d$group_id <- factor(d$group_id,levels=levels_group)
  d$repository <- factor(d$repository)
  d$merged <- as.integer(d$merged)
  d$test_file_presence <- as.integer(d$test_file_presence)
  d$files_c <- d$log1p_files_changed-mean(d$log1p_files_changed)
  d$description_c <- d$log1p_description_word_count-mean(d$log1p_description_word_count)
  support <- tapply(d$merged,d$repository,function(v) length(unique(v))==2L)
  d <- d[d$repository %in% names(support)[support],,drop=FALSE]
  droplevels(d)
}
rq1 <- prepare(rq1); rq2 <- prepare(rq2)
if (anyNA(rq1) || anyNA(rq2)) stop('Missing family-model data')

lrt <- function(full,reduced,label,role) {
  value <- max(0,2*(as.numeric(logLik(full))-as.numeric(logLik(reduced))))
  df <- attr(logLik(full),'df')-attr(logLik(reduced),'df')
  data.frame(test=label,analysis_role=role,likelihood_ratio=value,degrees_freedom=df,
             p_value=pchisq(value,df=df,lower.tail=FALSE))
}
term_name <- function(cf,parts) {
  candidates <- names(cf)[sapply(strsplit(names(cf),':',fixed=TRUE),function(x) length(x)==length(parts) && setequal(x,parts))]
  if (length(candidates)!=1) stop('Cannot identify term: ',paste(parts,collapse=':'))
  candidates[[1]]
}
linear <- function(fit,weights) {
  cf <- coef(fit); vc <- vcov(fit)
  b <- sum(weights*cf[names(weights)])
  s <- sqrt(as.numeric(t(weights)%*%vc[names(weights),names(weights),drop=FALSE]%*%weights))
  c(estimate=b,standard_error=s,odds_ratio=exp(b),confidence_low=exp(b-qnorm(.975)*s),
    confidence_high=exp(b+qnorm(.975)*s),wald_p=pchisq((b/s)^2,df=1,lower.tail=FALSE))
}
association <- function(fit,term,group) {
  cf <- coef(fit); w <- setNames(1,term)
  if (group!='human') {
    interaction <- term_name(cf,c(paste0('group_id',group),term))
    w <- setNames(c(1,1),c(term,interaction))
  }
  linear(fit,w)
}
write_fit_coefs <- function(fit,name) {
  tab <- as.data.frame(summary(fit)$coefficients); tab$term <- rownames(tab); rownames(tab) <- NULL
  write.csv(tab,file.path(out,name),row.names=FALSE)
}

rq1_full_formula <- merged ~ group_id + files_c + test_file_presence + description_c +
  group_id:files_c + group_id:test_file_presence + group_id:description_c + strata(repository)
rq1_fit <- clogit(rq1_full_formula,data=rq1,method='efron')
rq1_tests <- rbind(
  lrt(rq1_fit,clogit(update(rq1_full_formula,. ~ . - group_id:files_c),data=rq1,method='efron'),'files_changed_by_family','RQ1 interpretation'),
  lrt(rq1_fit,clogit(update(rq1_full_formula,. ~ . - group_id:test_file_presence),data=rq1,method='efron'),'test_file_presence_by_family','RQ1 interpretation'),
  lrt(rq1_fit,clogit(update(rq1_full_formula,. ~ . - group_id:description_c),data=rq1,method='efron'),'description_length_by_family','RQ1 interpretation'))
rq1_tests$p_value_holm <- p.adjust(rq1_tests$p_value,method='holm')
write.csv(rq1_tests,file.path(out,'rq1_family_omnibus_tests.csv'),row.names=FALSE)
rq1_assoc <- do.call(rbind,lapply(c('files_c','test_file_presence','description_c'),function(term) {
  do.call(rbind,lapply(levels_group,function(group) {
    data.frame(characteristic=term,group_id=group,t(association(rq1_fit,term,group)),row.names=NULL)
  }))
}))
write.csv(rq1_assoc,file.path(out,'rq1_family_associations.csv'),row.names=FALSE)
write_fit_coefs(rq1_fit,'rq1_family_coefficients.csv')

controls <- 'group_id + files_c + test_file_presence + description_c + group_id:files_c + group_id:test_file_presence + group_id:description_c'
formal_formula <- as.formula(paste('merged ~',controls,'+ formal_review + group_id:formal_review + strata(repository)'))
inline_formula <- as.formula(paste('merged ~',controls,'+ inline_review_comment + group_id:inline_review_comment + strata(repository)'))
joint_formula <- as.formula(paste('merged ~',controls,'+ formal_review + group_id:formal_review + inline_review_comment + group_id:inline_review_comment + strata(repository)'))
formal_fit <- clogit(formal_formula,data=rq2,method='efron')
inline_fit <- clogit(inline_formula,data=rq2,method='efron')
joint_fit <- clogit(joint_formula,data=rq2,method='efron')
rq2_tests <- rbind(
  lrt(formal_fit,clogit(update(formal_formula,. ~ . - group_id:formal_review),data=rq2,method='efron'),'formal_review_by_family','RQ2 family interpretation'),
  lrt(inline_fit,clogit(update(inline_formula,. ~ . - group_id:inline_review_comment),data=rq2,method='efron'),'inline_review_by_family','RQ2 secondary family interpretation'),
  lrt(joint_fit,clogit(update(joint_formula,. ~ . - group_id:inline_review_comment),data=rq2,method='efron'),'incremental_inline_by_family_given_formal','RQ2 secondary family interpretation'))
write.csv(rq2_tests,file.path(out,'rq2_family_omnibus_tests.csv'),row.names=FALSE)
rq2_assoc <- do.call(rbind,lapply(levels_group,function(group) {
  formal <- association(joint_fit,'formal_review',group)
  inline <- association(joint_fit,'inline_review_comment',group)
  rbind(data.frame(signal='formal_review_without_inline',group_id=group,t(formal),row.names=NULL),
        data.frame(signal='inline_given_formal',group_id=group,t(inline),row.names=NULL))
}))
write.csv(rq2_assoc,file.path(out,'rq2_family_joint_associations.csv'),row.names=FALSE)
write_fit_coefs(formal_fit,'rq2_family_formal_coefficients.csv')
write_fit_coefs(inline_fit,'rq2_family_inline_coefficients.csv')
write_fit_coefs(joint_fit,'rq2_family_joint_coefficients.csv')
write.csv(data.frame(rq1_rows=nrow(rq1),rq2_rows=nrow(rq2),repositories=nlevels(rq2$repository),groups=length(levels_group)),
          file.path(out,'population.csv'),row.names=FALSE)
writeLines(c(capture.output(sessionInfo()),paste('rq1_formula=',paste(deparse(rq1_full_formula),collapse=' ')),
             paste('rq2_joint_formula=',paste(deparse(joint_formula),collapse=' ')),
             'role=family analyses interpret RQ1 and RQ2; they are not a separate research question'),
           file.path(out,'session_and_specification.txt'))
cat('FAMILY_MODELS_COMPLETE\n')
