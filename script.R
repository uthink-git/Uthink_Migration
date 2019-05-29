library(ggplot2)
library(dplyr)
library(reshape2)
library(data.table)

setwd('data/')


format_long = function(input_data){
  temp_input = copy(input_data)
  long_data = as.data.frame(melt(setDT(temp_input, keep.rownames = TRUE), "rn"))
  colnames(long_data) = c('origine', 'annee','nombre')
  
  long_data$origine = as.factor(long_data$origine)
  long_data$annee = as.numeric(as.character(long_data$annee))
  return(long_data)
}

get_pct = function(input_data, tot_pop){
  data_pct = data.frame(t(apply(input_data, 1, function(x) x/tot_pop)))
  colnames(data_pct) = colnames(input_data)
  rownames(data_pct) = rownames(input_data)
  data_pct = data_pct * 100
  #colnames(data_pct) = c('origine', 'annee', 'pourcentage')
  return(data_pct)
}

immigration = read.csv('immigration_monde.csv', header = TRUE, row.names = 1)
years = 2002:2017
colnames(immigration) = years

emigration = read.csv('emigration_monde.csv', header = TRUE, row.names = 1)
colnames(emigration) = years

solde_migration = immigration-emigration
population_tot_suisse =as.numeric(c('7255700','7313900','7364100','7415100','7459100','7508700','7593500','7701900','7785800','7870100','7954700','8039100','8139600','8237700','8327100','8419600'))

immigration_pct = get_pct(immigration, population_tot_suisse)
emigration_pct = get_pct(emigration, population_tot_suisse)
solde_migration_pct = get_pct(solde_migration, population_tot_suisse)

#Formatage immigration
immigration_long = format_long(immigration)
immigration_long_pct = format_long(immigration_pct)

#Formatage émigration
emigration_long = format_long(emigration)
emigration_long_pct = format_long(emigration_pct)

#Formatage solde
solde_migration_long = format_long(solde_migration)
solde_migration_long_pct = format_long(solde_migration_pct)

save(file = '../migration_data.Rdata', emigration_long, emigration_long_pct, immigration_long, immigration_long_pct, solde_migration_long, solde_migration_long_pct, years, population_tot_suisse)

# ggplot(immigration_long,aes(x = Année, y=Nombre, fill = Nationalité)) + geom_area()
# ggplot(immigration_long_pct,aes(x = Année, y=Nombre, fill = Nationalité)) + geom_area() + scale_fill_viridis(discrete=TRUE) + theme_minimal() + geom_hline(aes(yintercept= 1, linetype = "Votre moyenne prédite entre 2002 et 2017"), colour= 'red') +
#   geom_hline(aes(yintercept= 2, linetype = "Votre souhait de solde migratoire"), colour= 'blue') +
#   scale_linetype_manual(name = "limit", values = c(2, 2), 
#                         guide = guide_legend(override.aes = list(color = c("red", "blue"))))
# 
# ggplot(immigration_long,aes(x = Année, y=Nombre, fill = Nationalité)) + geom_bar(stat = 'identity')
# ggplot(immigration_long_pct,aes(x = Année, y=Nombre, fill = Nationalité)) + geom_bar(stat = 'identity')
# 
# ggplot(emigration_long,aes(x = Année, y=Nombre, fill = Nationalité)) + geom_area()
# ggplot(emigration_long_pct,aes(x = Année, y=Nombre, fill = Nationalité)) + geom_area()
# ggplot(solde_migration_long,aes(x = Année, y=Nombre, fill = Nationalité)) + geom_area()
# 
# ggplot(solde_migration_long,aes(x = Année, y=Nombre, fill = Nationalité)) + geom_bar(stat = 'identity')
# ggplot(solde_migration_long_pct,aes(x = Année, y=Nombre, fill = Nationalité)) + geom_bar(stat = 'identity')
