rea#
# (c) 2014 -- onwards Georgios Gousios <gousiosg@gmail.com>
#
# BSD licensed, see LICENSE in top level dir
#

if (!"devtools" %in% installed.packages()) install.packages("devtools")
require(devtools)
if (!"likert" %in% installed.packages()) install_github("likert","gousiosg")

require(likert)
require(plyr)

column.mappings <- c(
"Which.repository.or.repositories.do.you.handle.pull.requests.for." = "Q1",
"What.is.your.favourite.PRioritizer.feature." = "Q2",
"Please.rate.the.usefulness.of.the.following.features...Automatic.ordering." = "Q3.A1",
"Please.rate.the.usefulness.of.the.following.features...Test.code.filtering." = "Q3.A2",
"Please.rate.the.usefulness.of.the.following.features...Target.branch.filtering." = "Q3.A3",
"Please.rate.the.usefulness.of.the.following.features...Pairwise.conflicts.inspection.sorting." = "Q3.A4",
"Please.rate.the.usefulness.of.the.following.features...Contributed.commits.inspection.sorting." = "Q3.A5",
"Please.rate.the.usefulness.of.the.following.features...Accept.rate.inspection.sorting." = "Q3.A6",
"Please.rate.the.usefulness.of.the.following.features...Size.inspection.sorting." = "Q3.A7",
"What.features.do.you.miss.from.the.PRioritizer."= "Q4",

"Please.rate.the.usability.of.the.PRioritizer.service...It.is.easy.to.get.an.overview.of.the.state.of.the.pull.requests.of.the.project." = "Q5.A1",
"Please.rate.the.usability.of.the.PRioritizer.service...It.is.easy.to.find.what.pull.request.to.work.on.next." = "Q5.A2",
"Please.rate.the.usability.of.the.PRioritizer.service...The.filter.lacks.support.for.some.important.fields." = "Q5.A3",
"Please.rate.the.usability.of.the.PRioritizer.service...Using.the.prioritizer.service.causes.too.much.overhead." = "Q5.A4",
"Please.rate.the.usability.of.the.PRioritizer.service...Some.pull.requests.show.incorrect.information." = "Q5.A5",

"What.do.you.like.the.most.about.the.PRioritizer.service." = "Q6",

"Please.rate.the.following.aspects.of.the.PRioritizer.service...I.would.use.prioritizer.for.my.project." = "Q7.A1",
"Please.rate.the.following.aspects.of.the.PRioritizer.service...I.would.recommend.it.to.others." = "Q7.A2",
"Please.rate.the.following.aspects.of.the.PRioritizer.service...I.like.the.prioritizer.web.interface.more.than.GitHub.s." = "Q7.A3",
"Please.rate.the.following.aspects.of.the.PRioritizer.service...The.performance.of.prioritizer.is.adequate." = "Q7.A4",
"Please.rate.the.following.aspects.of.the.PRioritizer.service...The.time.delay.between.GitHub.and.the.service.is.acceptable." = "Q7.A5",

"Please.rate.the.following.features.you.would.like.to.see.in.a.future.version...Prioritization.per.integrator.instead.of.per.repository..triage.." = "Q8.A1",
"Please.rate.the.following.features.you.would.like.to.see.in.a.future.version...Support.for.private.repositories." = "Q8.A2",
"Please.rate.the.following.features.you.would.like.to.see.in.a.future.version...Support.for.GitHub.labels." = "Q8.A3",
"Please.rate.the.following.features.you.would.like.to.see.in.a.future.version...Filter.on.pull.request.type..e.g..fix.refactor.feature.doc.." = "Q8.A4",
"Please.rate.the.following.features.you.would.like.to.see.in.a.future.version...I.would.like.to.have.more.control.over.the.prioritization..user.feedback.." = "Q8.A5",

"How.can.we.improve.the.PRioritizer.service.for.your.project." = "Q9",

"Would.you.be.available.for.a.short.interview.over.email..Skype.or.Google.Hangouts." = "Q10"

)

# Prepare data for ploting likert scale questions
plot.likert.data <- function(data, question, order = c(),  title = '',
                             group.by = '', mappings = column.mappings) {
  answers <- data[, find.answer.cols(data, question)]
  
  allowed.cols <<- Filter(function(x){length(grep("\\.other", x)) <= 0}, colnames(answers))
  answers <- answers[, allowed.cols]
  
  # Re-level factors according to the provided order
  for (col in colnames(answers)) {
    answers[,col] <- factor(answers[,col], order)
  }
  
  answers <- resolve.question.text(answers)
  if(nchar(group.by) == 0) {
    plot(likert(answers), centered=TRUE, wrap=15, legend.position='top',
         text.size = 2, title = title)
  } else {
    answers$groupping <- data[, group.by]
    plot(likert(subset(answers, select=-c(groupping)), grouping=answers$groupping),
         centered=TRUE, wrap=85, legend.position='top',
         title = title, grouping = answers$groupping)
  }
}

resolve.question.text <- function(df, mappings = column.mappings) {
  qs <- Map(function(x){
    # Reverse lookup of the question text in the  mapping table
    q.text <- search.list.by.value(mappings, x)
    # Get a formatted version of the answer text
    parse.sm.question.text(q.text)$answer
  }, colnames(df))
  
  rename(df, qs)
}

parse.sm.question.text <- function(x) {
  
  replace.chars <- function(x) {
    gsub(".", ' ', gsub("..", ", ", x, fixed = T), fixed = T)
  }
  
  # Get question part, identified by ...+
  q <- replace.chars(strsplit(x, "\\.\\.\\.+", )[[1]][1])
  
  # If a capital character appears after a comma, this is means 2 sentences in
  # question. Format second sentence starting character.
  q <- gsub(", ([A-Z])", ". \\1", q)
  
  # Append question mark
  q <- sprintf("%s?", q)
  
  a <- replace.chars(strsplit(x, "\\.\\.\\.+")[[1]][2])
  list(question = trim(q), answer = trim(a))
}

find.answer.cols <- function(data, question, other = TRUE) {
  if (other) {
    grep(sprintf("%s([\\.]|$)", question), colnames(data))
  } else {
    grep(sprintf("%s$", question), colnames(data))
  }
}

search.list.by.value <- function(l, val) {
  names(l[!is.na(match(l, val))])
}

store.pdf <- function(data, name, where = '../figs') {
  tryCatch({
    pdf(file.path(where, name))
    plot(data)
  }, finally = {dev.off()}
  );
}

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

prioritizer <- read.csv("responses-prioritizer.csv")
prioritizer <- rename(prioritizer, column.mappings)

p1 <- plot.likert.data(prioritizer, "Q3",
                 c("Useless", "Not so useful", "Useful", "Very useful"),
                 "Usefulness of Prioritizer features")

p2 <- plot.likert.data(prioritizer, "Q5", 
                 c("Strongly disgree", "Disagree", "Agree", "Strongly agree"),
                 "Usefulness of the Prioritizer service")

p3 <- plot.likert.data(prioritizer, "Q7", 
                 c("Strongly disgree", "Disagree", "Agree", "Strongly agree"),
                 "Aspects of the PRioritizer service")

p4 <- plot.likert.data(prioritizer, "Q8",
                 c("Useless", "Not so useful", "Useful", "Very useful"),
                 "Importance of future features")

store.pdf(p1, "feature-userfulness.pdf")
store.pdf(p2, "service-userfulness.pdf")
store.pdf(p3, "service-aspects.pdf")
store.pdf(p4, "future-features.pdf")
