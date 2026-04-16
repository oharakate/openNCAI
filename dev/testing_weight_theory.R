pot_weights <- data.frame(
  water = c(1, 0.5, 0.2),
  soil = c(0.4, 0.6, 1),
  culture = c(0.9, 0.7, 0.5),
  row.names = c("coast", "heath", "farm")
)

hab1 = c(1, 1, 1)
hab2 = c(1, 0.9, 1.1)
hab3 = c(1, 0.85, 1.15)

wb_weights <- c(0.4, 0.4, 0.2)

# Potential weights:
pot_weights
pot_weights * hab1
# Y1 habs are all 1 so potential weights should == pot weights * hab1:
all.equal(pot_weights, (pot_weights * hab1))

# Potential provision
pot_weights
# espb = hab1 * pot_weights:
espb = hab1 * pot_weights
# In year 2 expect water == 1, 0.45, 0.22, soil == 0.4, 0.54, 1.1, etc.:
espb * hab2
# And should change again in y3:
espb * hab3

# Potential wellbeing:
# This is calc as hab * wb_base,
#   where wb_base = hab1 * espb,
wb_base <- hab1 * espb
wb_base * hab1
wb_base * hab2
wb_base * hab3

# Would they diverge?
all.equal(espb* hab3, wb_base * hab3)
# They would not?

# Is it about the adjustment?
custom_weights <- data.frame(
  water = c(1, 1, 1),
  soil = c(0.5, 0.5, 0.5),
  culture = c(1, 1, 1),
  row.names = c("coast", "heath", "farm")
)
# I don't think it should be, because they happen at the very beginning of the
# process where the potential scores are converted into weights, so before the
# pot_weights here are generated.
# So repeating the above
custom_weights
# espb = hab1 * pot_weights:
espb2 = hab1 * custom_weights
# In year 2 expect water == 1, 0.45, 0.22, soil == 0.4, 0.54, 1.1, etc.:
espb2 * hab2
# And should change again in y3:
espb2 * hab3

wb_base2 <- hab1 * wb_weights
wb_base2
wb_base2 * hab2
wb_base2 * hab3

all.equal(espb2* hab3, wb_base2 * hab3)
# Not eaual, so this is about the fact that the espb isn't 1, because of the
# adjustments and that means we need to think about the meaning of that
# substantively.
# What we have actually done here is design a sensitivity analysis for their
# approach with the adjustment I guess.
# Although again, I'm still not totally on top of the logic here...
