weight_g <-  c(50, 60, 65, 82)

animals <- c("mouse", "rat", "dog")

#get the length of a vector
length(animals)

#get the type of data contained in the vector
class(animals)
class(weight_g)

#structure of the object
str(animals)

#how to add an element at beginning of vector
animals <- c("chinchilla", animals)
animals <- c(animals, "frog")

typeof(animals)

#what happens in the next cases
num_char <- c(1, 2, 3, "a")
num_logical <- c(1, 2, 3, TRUE)
char_logical <- c("a", "B", "c", TRUE)
tricky <- c(1, 2, 3, "4")
#logical -> numeric -> character
#numeric -> charcater
#logical -> character

#subsetting a vector
animals[2]
animals[c(1, 2)]

more_animals <- animals[c(1, 2, 3, 2, 1, 4)]

weight_g
weight_g[c(FALSE, FALSE, TRUE, TRUE)]
weight_g > 63
weight_g[weight_g>63]
weight_g[weight_g>63 & weight_g<80]
weight_g[weight_g<58 | weight_g>80]
# <, >, ==, !=, <=, >= 

animals[animals == "rat" | animals == "frog"]
# %in% helps us find all elements corresponding to a vector of our choice
animals %in% c("rat", "frog", "cat", "duck", "dog")
animals[animals %in% c("rat", "frog", "cat", "duck", "dog")]

# an example of a vector with missing data
heights <- c(2, 4, 4, NA, 6)
mean(heights)
mean(heights, na.rm = T)
max(heights, na.rm = T)

# identify the missing data
is.na(heights)
heights[!is.na(heights)]
# omit the missing data
na.omit(heights)
# extract the complete cases
complete.cases(heights)
heights[complete.cases(heights)]

# challenge my try
heights_challenge <- c(63, 69, 60, 65, NA, 68, 61, 59, 64, 69, 63)
heights_no_na <- c(heights_challenge[!is.na(heights_challenge)])
median(heights_no_na)
sum(heights_no_na>67)

# challenge correction
heights <- c(63, 69, 60, 65, NA, 68, 61)
heights_no_na <- heights[!is.na(heights)

median(heights, na.rm =T)

heights_no_na[heights_no_na > 67]
length(heights_no_na[heights_no_na > 67])

