lindh_sample = read.csv("downloads/lindh.csv");
lindh_sample$has_design = as.logical(lindh_sample$is_treatment);

lindh_sample$continuous_integration = as.logical(lindh_sample$continuous_integration);
lindh_sample$license = as.logical(lindh_sample$license);
lindh_sample$language = as.factor(lindh_sample$language);
lindh_sample$language <- relevel(lindh_sample$language, ref = "Java");

lm1 = lm(log(num_issues_classifier+1) ~
           log(age+1) +
           log(num_commits) +
           log(num_contributors) +
           log(stars+1) +
           log(unit_test+1) +
           documentation +
           continuous_integration +
           license +
           language +
           
           has_design,
         data = lindh_sample)

vif(lm1)
summary(lm1)
anova(lm1)
plot(lm1)