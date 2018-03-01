number_of_unique_question = nrow(question)
number_of_questions = number_of_unique_question ^ 2
questions_for_word2vec = matrix("", number_of_questions, 2)
k = (number_of_unique_question - 2)
h = 1:number_of_unique_question
for (i in 1:number_of_unique_question)
{
  j = k * (i - 1) + i
  questions_for_word2vec[j:(j + k), 1] = question[i, 1][[1]]
  questions_for_word2vec[j:(j + k), 2] = question[h[-i], 1][[1]]
}
