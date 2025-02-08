# Define team members' information using a data frame
team <- data.frame(
  Name = c("Alice Johnson", "Bob Smith", "Charlie Lee", "David Kim", "Eva Martinez"),
  Slack_Username = c("@alice", "@bob", "@charlie", "@david", "@eva"),
  Email = c("alice@example.com", "bob@example.com", "charlie@example.com", "david@example.com", "eva@example.com"),
  Hobby = c("Reading", "Hiking", "Gaming", "Photography", "Cooking"),
  Country = c("USA", "Canada", "UK", "South Korea", "Spain"),
  Discipline = c("Computer Science", "Data Science", "Machine Learning", "Bioinformatics", "Software Engineering"),
  Preferred_Language = c("Python", "R", "Python", "R", "Python")
)

# View the data frame
print(team)

write.csv(team, "team_info.csv", row.names = FALSE)
