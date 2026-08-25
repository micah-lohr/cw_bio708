## 8/25/2026 script02_data_manipulation
#install.packages("tidyverse") once you install it you don't have to do it again
library(tidyverse)

set.seed(123)

iris_sub <- as_tibble(iris) %>% 
  group_by(Species) %>% 
  sample_n(3) %>% 
  ungroup()

print(iris_sub)

#Filter 
filter(iris_sub, Species=="virginica")

filter(iris_sub, Species != "virginica") # != means "does not equal"

filter(iris_sub, Species %in% c("virginica", "versicolor")) # %in% is for multiple matches, create a vector after

filter(iris_sub, Species == "virginica" | Species == "versicolor")

filter(iris_sub, Sepal.Width > 5 | Species== "versicolor") 


#Arrange
arrange(iris_sub, Sepal.Length) #default is ascending order 

arrange(iris_sub, desc(Sepal.Length))


# 5.1.3 Exercise: using iris_sub dataframe, select rows that match the following contidions
# (1) Sepal.Width is greater than 3.0 and assign the new dataframe to iris_3
iris_3 <- filter(iris_sub, Sepal.Width >3)
print(iris_3)

# (2) Species is "setosa" and assign the new dataframe to iris_setosa
iris_setosa <- filter(iris_sub, Species == "setosa")
print(iris_setosa)

# (3) Sepal.Width is greater than 3.0 AND Species is "setosa", and assign the new dataframe to iris_3_setosa
iris_3_setosa <- filter(iris_sub, Sepal.Width > 3 & Species == "setosa")
print(iris_3_setosa)


#Select- works for columns, whereas filter() is for rows
select(iris_sub, Sepal.Length) #select one column 

select(iris_sub, c(Sepal.Length, Sepal.Width)) #select multiple columns by making a vector

select(iris_sub, -Sepal.Length) #remove one column

select(iris_sub, -c(Sepal.Length, Sepal.Width)) #remove multiple columns 

select(iris_sub, starts_with("Sepal")) #select or remove columns that start with a certain word

select(iris_sub, -starts_with("Sepal")) #remove with "starts_with" shown here

select(iris_sub, ends_with("Width")) #same idea of selecting or removing with "ends_with"

select(iris_sub, -ends_with("Sepal"))


#Mutate 
x_max <- nrow(iris_sub) #see number of rows in df
x <- 1:x_max #create a vector from 1 to x_max
mutate(iris_sub, row_id=x) #add new column to dataframe with variable name "row_id" and values from x

mutate(iris_sub, sl_two_times = 2 * Sepal.Length) # double the "Sepal.Length" values and add as a new column

mutate(iris_sub, id_name = paste(Species, Sepal.Length)) #As a bonus, Jonah showed us the paste() function


#5.2.3 Exercise: Using iris_sub dataframe, select columns that match the following contidions
# (1) Select column Petal.Width and Species and assign the new dataframe to iris_pw
iris_pw <- select(iris_sub, c(Petal.Width, Species))
print(iris_pw)

# (2) Select columns starting with text "Petal" and assign the new dataframe to iris_petal
iris_petal <- select(iris_sub, starts_with("Petal"))
print(iris_petal)

# (3) Add new column pw_two_times by doubling values in column Petal.Width, and assign the new dataframe to iris_pw_two
iris_pw_two <- mutate(iris_sub, pw_two_times = 2*Petal.Width)
print(iris_pw_two)


#Piping 
## Example without piping:
df_vir <- filter(iris_sub, Species == "virginica")
df_vir_sl <- select(df_vir, Sepal.Length)
print(df_vir_sl)

## Example with piping:
df_vir_sl <- iris_sub %>% 
  filter(Species == "virginica") %>% 
  select(Sepal.Length)
print(df_vir_sl)

#5.3.2 Exercise: Subset iris_sub by Species column (choose only "setosa" ) and add a new column pw_two_times by doubling values in column Petal.Width. 
#Assign the resultant dataframe to iris_pipe. USE pipe %>% in this operation.
iris_pipe <- filter(iris_sub, Species=="setosa") %>%
  mutate(pw_two_times=2*Petal.Width)
print(iris_pipe)


#Group Operation 
iris_sub %>% 
  group_by(Species) #notice in the console, the extra note at the top says Groups: Species [3]

iris_sub %>% 
  group_by(Species) %>% 
  summarize(mu_sl = mean(Sepal.Length)) #the summarize() function can give you basic summary stats for your data 

iris_sub %>% 
  group_by(Species) %>% 
  summarize(mu_sl = mean(Sepal.Length),
            sum_sl = sum(Sepal.Length)) #calculate multiple summary stats by separating with a comma

# grouping by "Species", then take means "Sepal.Length" for each species
iris_sub %>% 
  group_by(Species) %>% 
  mutate(mu_sl = mean(Sepal.Length)) %>% 
  ungroup() #make sure to ungroup to prevent errors in subsequent operations

#Reshape- did on my own time
#Use a pipeline to make the dataframe wider with pivot_wide()
iris_w <- iris_sub %>% 
  mutate(id = rep(1:3, 3)) %>% # add an ID column
  select(id, Sepal.Length, Species) %>% 
  pivot_wider(id_cols = "id", # unique row ID 
              values_from = "Sepal.Length", # values in each cell from this
              names_from = "Species") # new column names from this
print(iris_w)

#Use a pipeline to make the dataframe longer with pivot_long()
iris_l <- iris_w %>% 
  pivot_longer(cols = c("setosa",
                        "versicolor",
                        "virginica"), # columns with values to be reshaped
               names_to = "Species", # column IDs move to "Species"
               values_to = "Sepal.Length") # column values move to "Sepal.Length"

print(iris_l)


#Join- did on my own time
## matching by a single column
## left join by "Species": one to one
df1 <- tibble(Species = c("A", "B", "C"),
              x = c(1, 2, 3))

df2 <- tibble(Species = c("A", "B", "C"),
              y = c(4, 5, 6))

left_join(x = df1,
          y = df2,
          by = "Species")

# matching by a single column
## left join by "Species": one to many
df3 <- tibble(Species = c("A", "A", "B", "C"),
              y = c(4, 5, 6, 7))

left_join(x = df1,
          y = df3,
          by = "Species")

# matching by a single column
## left join by "Species": one to missing
df4 <- tibble(Species = c("A", "A", "C"),
              y = c(4, 5, 7))

left_join(x = df1,
          y = df4,
          by = "Species")

# matching by multiple columns
## one to one
df5 <- tibble(Species = c("A", "B", "C"),
              x = c(1, 2, 3),
              z = c("cool", "awesome", "magical"))

left_join(x = df1,
          y = df5,
          by = c("Species", "x"))

# matching by multiple columns
## one to many
df6 <- tibble(Species = c("A", "A", "B", "C"),
              x = c(1, 1, 2, 3),
              z = c("cool", "cool", "awesome", "magical"))

left_join(x = df1,
          y = df6,
          by = c("Species", "x"))

# matching by multiple columns
## one to missing
df6 <- tibble(Species = c("A", "B", "C"),
              x = c(1, 2, 4),
              z = c("cool", "awesome", "magical"))

left_join(x = df1,
          y = df6,
          by = c("Species", "x"))
