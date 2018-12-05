{- Simple HTML String Templates
   H. Conrad Cunningham

1234567890123456789012345678901234567890123456789012345678901234567890

2018-11-06: Original version

-}

module SimpleHTML
    ( HTML, ListType(..), LangType(..),
      to_html, to_body, to_heading, to_list, to_li
    )
where

type HTML     = String
data ListType = Decimal | UpRoman | LowRoman
              | UpLettered | LowLettered | Bulleted
                deriving (Eq, Show)
data LangType = English | Spanish | Portuguese | French
                deriving (Eq, Show)

langmap = [ (English,"en"), (Spanish,"es"), (Portuguese,"pt"),
            (French,"fr") ]

-- Wraps HTML _content_ in <html></html> tags with _lang_ attribute
-- set to _langtype_, defaulting to English
to_html :: LangType -> HTML -> HTML
to_html langtype content =
    "<html lang=\"" ++ lang ++ "\">" ++ content ++ "</html>"
        where lang = case lookup langtype langmap of
                         Just l  -> l
                         Nothing -> "en"

-- Wraps HTML _content_ in <body></body> tags
to_body :: HTML -> HTML
to_body content = "<body>" ++ content ++ "</body>"

-- Wraps String _title_ in <hN> </hN> tags for N in [1..6], taking
-- nearest valid value for level if outside range
to_heading:: Int -> String -> HTML
to_heading level title = open ++ title ++ close
    where lev   = show (min (max level 1) 6)
          open  = "<h"  ++ lev ++ ">"
          close = "</h" ++ lev ++ ">"

-- Wraps HTML _content_ in <ul></ul> or <ol></ol> tags with _type_
-- attribute of <ol> set based _listtype_
to_list :: ListType -> HTML -> HTML
to_list listtype content = open ++ content ++ close
    where
        (open,close) =
            case listtype of
                Decimal     -> ("<ol type=\"1\">", "</ol>")
                UpRoman     -> ("<ol type=\"I\">", "</ol>")
                LowRoman    -> ("<ol type=\"i\">", "</ol>")
                UpLettered  -> ("<ol type=\"A\">", "</ol>")
                LowLettered -> ("<ol type=\"a\">", "</ol>")
                Bulleted    -> ("<ul>",            "</ul>")

-- Wraps HTML _content_ in <li></li> tags
to_li :: HTML -> HTML
to_li content = "<li>" ++ content ++ "</li>"


