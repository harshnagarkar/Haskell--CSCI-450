{- CSci 450/503, Fall 2018
   Homework #4:  Harsh Nagarkar
   10616659
-}

module ExamDSL where

-- instructor used these in his solution
import Data.List
import Data.Char
import Data.List ( delete, intercalate, intersect , nub)

import SimpleHTML (
      HTML, ListType(..), LangType(..),
      to_html, to_body, to_heading, to_list, to_li
    )

type Tag      = String
type QText    = String
data Question = Ask [Tag] QText [Choice] deriving Show

type AText    = String
data Choice   = Answer AText Bool deriving (Eq, Show)

type Title    = String
data Exam     = Quiz Title [Question] deriving Show

-- Exercise Set A

-- #A1
-- gets bool value of choice list
correctChoice :: Choice -> Bool
correctChoice (Answer atext b) = b

-- #A2
--getts the len of question string
lenQuestion :: Question -> Int
lenQuestion (Ask tag qtext choice) = length choice

-- #A3
-- based on length of question no of choices and one correct choice the question is validated
validQuestion :: Question -> Bool
validQuestion (Ask  tag qtext choice) = ans
  where
    qtextcheck = if(length qtext) /= 0
        then True
      else False
    choicecheck = if(length choice) >2 && (length choice<11)
        then True
      else False
    choicevalue = map correctChoice choice
    cvalue = if length (filter (\x-> x== True) choicevalue) == 1
        then True
      else False
    ans = choicecheck && cvalue && qtextcheck
    

-- #A4
-- checks if a question has a particular tag when searching
hasTag :: Question -> Tag -> Bool
hasTag (Ask  tag qtext choice) t= ans
  where
    ans = if length (filter( == t) tag) /=0
        then True
      else False

-- #A5
-- this is general equal argument checker function
eqBag :: Eq a => [a] -> [a] -> Bool
eqBag [] [] = True
eqBag [] _ =False
eqBag (x:xs) y = if length y > 0 
    then  eqBag xs (delete x y)
  else  False

-- #A6
-- this is a instance of above function for checking equality
instance Eq Question where
  (Ask tag1 q1 ch1) == (Ask tag2 q2 ch2) = eqBag tag1 tag2 && (eqBag q1 q2) && eqBag ch1 ch2 

-- #A7
-- recursive gets tags through question called by select tag
helperselectByTags :: [Tag] -> [Question]->[Question]
helperselectByTags []  _  = [] 
helperselectByTags (x:xs) q = ans
  where 
   checktag q = hasTag q x
   ans = (filter checktag q)++(helperselectByTags xs q)
   
-- getts select question and convertes to exam using above helper question
selectByTags :: [Tag] -> Exam -> Exam
selectByTags t (Quiz tt q) = (Quiz tt  (nub (helperselectByTags t q))) 

-- #A8
-- checks for exam validatity using valid question function
validExam :: Exam -> Bool
validExam (Quiz tt q) = ans 
    where 
      ans = (all (==True) (map validQuestion q))

-- creates a answer key for exam
makeKey :: Exam -> [(Int,Char)]
makeKey (Quiz tt q) = ans
  where
    ans = zip [1..((length q))] (helperchoice q)
  
-- gets correct choice for each question and helps above function
helperchoice:: [Question] -> [Char]
helperchoice [] = []
helperchoice ((Ask tag qtext choice) :xs) = ans
    where
      choicelst = map correctChoice choice
      index = case elemIndex (True) choicelst  of Just p->p 
      ans = (chr (index+65)): helperchoice (xs)
-- #A9

-- Exercise Set B
-- makeKey :: Exam -> [(Int,Char)]
--this is instructor
-- might be useful
newline :: String
newline = "\n"

-- might be useful
block :: HTML -> HTML
block str = newline ++ str ++ newline

choice2html :: Choice -> HTML
choice2html (Answer text _) = to_li text

-- #B1
-- this is my code to convert question to html
question2html :: Question -> HTML
question2html (Ask tg qt ch) = ans
    where
      ans = to_list UpLettered qt ++ concatMap choice2html ch

-- sample data/testcases
--  writeFile "output.html" $ question2html e0

-- #B2
-- exam2html :: Exam -> HTML

-- Test data given in description
-- q0 = Ask ["curriculum"]
--          "Which one of the following is a required course?"
--          [ Answer "CSci 323" False, 
--            Answer "CSci 450" True, 
--            Answer "CSci 525" False ]

-- q1 = Ask ["curriculum"]
--           "Which one of the following is a required course?"
--            [ Answer "CSci 323" False, 
--              Answer "CSci 450" True, 
--              Answer "CSci 525" False ]     
-- e0 = Quiz "Curriclum Test" [
--          Ask ["curriculum"]
--              "Which one of the following courses is required?"
--              [ Answer "CSci 323" False, 
--                Answer "CSci 450" True, 
--                Answer "CSci 525" False ],
--          Ask ["language","course"]
--              "What one of the following is used in CSci 450?"
--              [ Answer "Lua" False,
--                Answer "Elm" False,
--                Answer "Haskell" True ]
--          ]
