-- Harsh Nagarkar
-- 10616659
module ExamDSLTest where
import ExamDSL
--These are the test datas to test for the functions created
q0 = Ask ["curriculum"] "Which one of the following is a required course?" [Answer "CSci 323" False, Answer "CSci 450" True,Answer "CSci 525" False ]
e0 = Quiz "Curriclum Test" [Ask ["curriculum"] "Which one of the following courses is required?" [Answer "CSci 323" False,Answer "CSci 450" True,Answer "CSci 525" False ],Ask ["language","course"] "What one of the following is used in CSci 450?" [ Answer "Lua" False,Answer "Elm" False, Answer "Haskell" True ]]