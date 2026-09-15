#PROBLEM 1
student_id <- c("S01", "S02", "S03", "S04", "S05", "S06")
section <- c("A", "B", "A", "B", "A", "B")
quiz1 <- c(82, 91, 76, 88, 95, 69)
quiz2 <- c(85, 89, 80, 92, 94, 74)
passed <- c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE)

#PART A

#STEP 1
section <- factor(section, levels = c("A","B"))

#STEP 2
students <- data.frame(student_id, section, quiz1, quiz2, passed)

#STEP 3
score_matrix <- cbind(quiz1, quiz2)
rownames(score_matrix) <- student_id
colnames(score_matrix) <- c("quiz1", "quiz2")

#STEP 4
course_record <- list(
  course = "R Programming",
  scores = students,
  cutoffs = c(pass = 70, excellent = 90)
)

#A vector stores one type of data in a single dimension.
#A matrix stores one type of data in two dimensions.
#A data frame stores multiple types of data in tabular form.
#A list stores objects of any type or structure.A factor stores categorical data with fixed levels.


#PART B

#STEP 1
score_matrix["S04", "quiz2"]

#STEP 2
score_matrix[1:2, , drop = FALSE]

#STEP 3
course_record["course"]     

#STEP 4
course_record[["course"]]   

#STEP 5
course_record$course       

#STEP 6
# [ returns a subset but keeps the original structure.
# [[ extracts a single element and drops structure.
# $ extracts list/data‑frame elements by name.


#PART C

#STEP 1
students$average <- (students$quiz1 + students$quiz2) / 2

#STEP 2
students$excellent <- students$average >= 90

#STEP 3
subset_A80 <- students[students$section == "A" & students$average >= 80, ]

#STEP 4
subset_A80[c("student_id","section","average")]

#STEP 5
avg_vec <- students$average
names(avg_vec) <- students$student_id

#Vectorization = one command, many operations.R applies the operation to each element without loops.


#PROBLEM 2
csv_text <- "sample_id,site,temp_c,ph,status
M01,North,18.2,7.1,ok
M02,South,20.5,,ok
M03,North,NA,6.8,review
M04,East,22.1,7.4,ok
M05,South,19.7,7.0,review
M06,East,23.0,NA,ok
M07,North,17.8,6.9,ok
M08,South,21.2,7.2,ok"


#PART A

#STEP 1
measurements <- read.csv(text = csv_text, na.strings = c("", "NA"))
measurements

#STEP 2
str(measurements)

#STEP 3
colSums(is.na(measurements))

#STEP 4
measurements_complete <- measurements[complete.cases(measurements), ]
measurements_complete

#STEP 5
removed <- measurements$sample_id[!complete.cases(measurements)]
removed

#STEP 6
#x == NA is not a valid missing-value test because comparisons with NA return NA rather than TRUE or FALSE.
#The is.na() function should be used to test whether a value is missing.

#PART B

#STEP 1
measurements$site <- factor(measurements$site)
measurements$status <- factor(measurements$status)

levels(measurements$site)
levels(measurements$status)

#STEP 2
measurements$temp_f <- measurements$temp_c * 9/5 + 32
measurements$temp_f

#STEP 3
measurements$ph_below_7 <- measurements$ph < 7
measurements$ph_below_7

#STEP 4
subset_NS_ok <- measurements[
  complete.cases(measurements) &
    measurements$site %in% c("North","South") &
    measurements$status == "ok",
]
subset_NS_ok

#STEP 5
subset_NS_ok[c("sample_id","site","temp_c","temp_f","ph")]

#STEP 6
mean(measurements$temp_c, na.rm = TRUE)
mean(measurements$temp_c[measurements$site == "South"], na.rm = TRUE)

#PART C

#STEP 1
A <- matrix(1:4, nrow = 2)
B <- matrix(5:8, nrow = 2)

#STEP 2
element_result <- A * B
element_result

#STEP 3
matrix_result <- A %*% B
matrix_result

#STEP 4
dim(element_result)
dim(matrix_result)

#STEP 5
 #The * operator performs element-wise multiplication, so values in 
 #matching positions in A and B are multiplied together.
 #The %*% operator performs matrix multiplication by multiplying 
 #rows by columns and adding the products. 


#PROBLEM 3
student_id <- paste0("P", sprintf("%02d", 1:8))
scores <- c(95, 82, NA, 67, 74, 88, 59, 91)

#PART A

#STEP 1
grade_one <- function(score, a_min = 90, b_min = 80, c_min = 70, d_min = 60) {}

#STEP 2
grade_one <- function(score, a_min = 90, b_min = 80, c_min = 70, d_min = 60) {if (is.na(score)){return(NA_character_)}}

#STEP 3
grade_one <- function(score, a_min = 90, b_min = 80, c_min = 70, d_min = 60) {
  if (is.na(score)) {
    return(NA_character_)
  } else if (score >= a_min) {
    return("A")
  } else if (score >= b_min) {
    return("B")
  } else if (score >= c_min) {
    return("C")
  } else if (score >= d_min) {
    return("D")
  } else {
    return("F")
  }
}

#STEP 4
grade_one(NA)
grade_one(90)
grade_one(80)
grade_one(85)
grade_one(74)

#PART B

#STEP 1
grades <- rep(NA_character_, length(scores))

#STEP 2
for (i in seq_along(scores)) {grades[i] <- grade_one(scores[i])}

#STEP 3
names(grades) <- student_id

#STEP 4
 #During the loop, i represents the position of the current score and grade in their vectors. 

#PART C-1

#STEP 1
summarize_scores <- function(x, na.rm = TRUE, digits = 1) {
  c(
    total = length(x),
    missing_count <- sum(is.na(x)),
    mean_score <- mean(x, na.rm = na.rm),
    sd_score <- sd(x, na.rm = na.rm),
    min_score <- min(x, na.rm = na.rm),
    max_score <- max(x, na.rm = na.rm)
  )
}

#STEP 2
summarize_scores(scores)

#STEP 3
summarize_scores(x = scores, na.rm = TRUE, digits = 2)

#PART C-2

#STEP 1
plot_scores <- function(x, ...) {plot(seq_along(x), x, ...)}

#STEP 2
plot_scores(scores, type = "b", pch = 19, xlab = "Position", ylab = "Score", main = "Student Scores")

#STEP 3
 #The graph plots each score against its position in the scores vector. 
 #The argument type = "b" displays both points and lines, and pch = 19 uses solid circle. 
 #Position 3 is missing because its score is NA, so no point is plotted there. 
 #The ... argument allows additional plotting options to be passed from plot_scores() to plot(). 



