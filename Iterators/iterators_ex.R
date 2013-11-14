library(iterators)

name = c("Bob", "Mary", "Jack", "Jane")

iname <- iter(name)

nextElem(iname)

people = data.frame(name, ages = c(17, 23, 41, 19), gender = rep(c("M", "F"), 2))

ipeople <- iter(people)

nextElem(ipeople)

ipeople <- iter(people, by='row')

nextElem(ipeople)
