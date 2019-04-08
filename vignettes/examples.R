# data(relationship_satisfaction)
# data(graduate_income)
# a = graduate_income
# d = relationship_satisfaction
# d = d[-which(d$gender=="Mantis religiosa"),]
# head(d)


		# ### simple regression
# flexplot(satisfaction~honesty, data=d)

		# ### t-test
# flexplot(satisfaction~gender, data=d)

		# ### anova
# flexplot(Income~Profession, data=a)		

		# ### factorial anova
# flexplot(Income~Profession + Grad.School, data=a)		
# flexplot(Income~Profession | Grad.School, data=a)		

		# ## ancova
# flexplot(Income~GPA + Grad.School, data=a, method="lm")
		# ## add a ghost line
# flexplot(Income~GPA | Grad.School, data=a, method="lm", ghost.line="gray",ghost.reference=list(Grad.School="No"))

		# ## related t
# data(alcuse)
# b = alcuse[-which(alcuse$AGE==15),]
# b$AGE = factor(b$AGE)
# q = flexplot(ALCUSE~AGE, data=b, related=T)
# q2 = flexplot(ALCUSE~AGE, data=b, related=F)
# cowplot::plot_grid(q, q2)

		# ## multiple regression
# flexplot(satisfaction~honesty | communication + interests, data=d, se=F, method="lm", ghost.line="gray",ghost.reference=list(communication=30, interests=55), bins=3)

		# ## mixed models
# require(lme4)		
# data(math)
# rand.intercept = lmer(MathAch~ SES + (SES|School), data=math)
# mixed.mod.visual(MathAch~SES, data=math, rand.intercept)	