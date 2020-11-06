{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Spec.Wordpress (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

import Data.DateTime (fromGregorian)
import qualified Data.Text as T
import qualified Text.RSS.Syntax as RSS
import qualified Text.XML.Light as XML

import Hakyll.Convert.Wordpress
import Hakyll.Convert.Common (DistilledPost(..))

deriving instance Eq DistilledPost
deriving instance Show DistilledPost

tests :: TestTree
tests = testGroup "Wordpress.distill"
  [
    extractsPostUri
  , extractsPostBody
  , combinesMultipleContentTags
  , extractsPostTitle
  , canSkipComments
  , canExtractComments
  , usesTheFirstCommentAuthorTag
  , turnsIncorrectDatesIntoEpochStart
  , parsesDates
  , extractsPostTags
  , extractsPostCategories
  ]

extractsPostUri :: TestTree
extractsPostUri =
  testGroup
    "extracts post's item link"
    [ testCase (T.unpack uri) (dpUri (distill False (createInput uri)) @?= uri)
    | uri <-
        [ "https://example.com/testing-post-uris"
        , "http://www.example.com/~joe/posts.atom"
        ]
    ]
  where
  createInput uri = (RSS.nullItem "First post")
    { RSS.rssItemLink = Just $ T.unpack uri
    }

contentTag :: XML.QName
contentTag =
  XML.QName
    { XML.qName   = "encoded"
    , XML.qURI    = Just "http://purl.org/rss/1.0/modules/content/"
    , XML.qPrefix = Just "content"
    }

extractsPostBody :: TestTree
extractsPostBody =
  testGroup
    "extracts post's body"
    [ testCase body (dpBody (distill False (createInput body)) @?= T.pack (body ++ "\n"))
    | body <-
        [ "<p>Today was a snowy day, and I decided to...</p>"
        , "<h3>My opinion on current affairs</h3><p>So you see, I...</p>"
        ]
    ]
  where
  createInput body = (RSS.nullItem "Test post")
    { RSS.rssItemOther =
        [
          XML.blank_element
            { XML.elName = contentTag
            , XML.elContent =
                [ XML.Text
                    (XML.blank_cdata
                      { XML.cdVerbatim = XML.CDataVerbatim
                      , XML.cdData = body
                      })
                ]
            }
        ]
    }

combinesMultipleContentTags :: TestTree
combinesMultipleContentTags =
  testCase
    "combines multiple content:encoded tags into the post body"
    (dpBody (distill False input) @?= T.pack (unlines [body1, body2]))
  where
  body1 = "<h3>Welcome!</h3>"
  body2 = "<p>Hope you like my blog</p>"

  input = (RSS.nullItem "Just testing")
    { RSS.rssItemOther =
        [ createElement body1
        , createElement body2
        ]
    }
  createElement body =
    XML.blank_element
      { XML.elName = contentTag
      , XML.elContent =
          [ XML.Text
              (XML.blank_cdata
                { XML.cdVerbatim = XML.CDataVerbatim
                , XML.cdData = body
                })
          ]
      }

extractsPostTitle :: TestTree
extractsPostTitle =
  testGroup
    "extracts post's title"
    [ testCase title (dpTitle (distill False (RSS.nullItem title)) @?= Just (T.pack title))
    | title <-
        [ "First post"
        , "You won't believe what happened to me today"
        , "Trying out <i>things</i>&hellip;"
        ]
    ]

commentTag :: XML.QName
commentTag =
  XML.QName
    { XML.qName = "comment"
    , XML.qURI = Just "http://wordpress.org/export/1.2/"
    , XML.qPrefix = Just "wp"
    }

commentContentTag :: XML.QName
commentContentTag =
  XML.QName
    { XML.qName = "comment_content"
    , XML.qURI = Just "http://wordpress.org/export/1.2/"
    , XML.qPrefix = Just "wp"
    }

commentDateTag :: XML.QName
commentDateTag =
  XML.QName
    { XML.qName = "comment_date"
    , XML.qURI = Just "http://wordpress.org/export/1.2/"
    , XML.qPrefix = Just "wp"
    }

commentAuthorTag :: XML.QName
commentAuthorTag =
  XML.QName
    { XML.qName = "comment_author"
    , XML.qURI = Just "http://wordpress.org/export/1.2/"
    , XML.qPrefix = Just "wp"
    }

canSkipComments :: TestTree
canSkipComments =
  testCase
    "does not extract comments if first argument is False"
    (dpBody (distill False input) @?= "<p>Hello, world!</p>\n")
  where
  input = (RSS.nullItem "Testing...")
    { RSS.rssItemOther =
        [ XML.blank_element
            { XML.elName = contentTag
            , XML.elContent =
                [ XML.Text
                    (XML.blank_cdata
                      { XML.cdVerbatim = XML.CDataVerbatim
                      , XML.cdData = "<p>Hello, world!</p>"
                      })
                ]
            }

        , XML.blank_element
            { XML.elName = commentTag
            , XML.elContent =
                [ XML.Text
                    (XML.blank_cdata
                      { XML.cdVerbatim = XML.CDataVerbatim
                      , XML.cdData = "<p>I'd like to point out that...</p>"
                      })
                ]
            }
        ]
    }

canExtractComments :: TestTree
canExtractComments =
  testGroup "extracts comments if first argument is True"
    [ noDateNoAuthor
    , dateNoAuthor
    , noDateAuthor
    , dateAuthor
    ]

  where
  createInput comment = (RSS.nullItem "Testing...")
    { RSS.rssItemOther =
        [ XML.blank_element
            { XML.elName = contentTag
            , XML.elContent =
                [ XML.Text
                    (XML.blank_cdata
                      { XML.cdVerbatim = XML.CDataVerbatim
                      , XML.cdData = "<p>Is this thing on?</p>"
                      })
                ]
            }

        , comment
        ]
    }

  noDateNoAuthor =
    testCase
      "comments with no \"published\" date and no author"
      (dpBody (distill True (createInput noDateNoAuthorComment)) @?= expectedNoDateNoAuthor)
  noDateNoAuthorComment =
    XML.blank_element
      { XML.elName = commentTag
      , XML.elContent =
          [ XML.Elem $ XML.blank_element
              { XML.elName = commentContentTag
              , XML.elContent =
                  [ XML.Text
                      (XML.blank_cdata
                        { XML.cdVerbatim = XML.CDataVerbatim
                        , XML.cdData = "<p>hi</p>"
                        })
                  ]
              }
          ]
      }
  expectedNoDateNoAuthor =
    "<p>Is this thing on?</p>\n\n\n\
    \<h3 id='hakyll-convert-comments-title'>Comments</h3>\n\
    \<div class='hakyll-convert-comment'>\n\
    \<p class='hakyll-convert-comment-date'>On unknown date, unknown author wrote:</p>\n\
    \<div class='hakyll-convert-comment-body'>\n\
    \<p>hi</p>\n\
    \</div>\n\
    \</div>"

  dateNoAuthor =
    testCase
      "comments with a \"published\" date but no author"
      (dpBody (distill True (createInput dateNoAuthorComment)) @?= expectedDateNoAuthor)
  dateNoAuthorComment =
    XML.blank_element
      { XML.elName = commentTag
      , XML.elContent =
          [ XML.Elem $ XML.blank_element
              { XML.elName = commentContentTag
              , XML.elContent =
                  [ XML.Text
                      (XML.blank_cdata
                        { XML.cdVerbatim = XML.CDataVerbatim
                        , XML.cdData = "<p>hi</p>"
                        })
                  ]
              }
          , XML.Elem $ XML.blank_element
              { XML.elName = commentDateTag
              , XML.elContent =
                  [ XML.Text
                      (XML.blank_cdata
                        { XML.cdVerbatim = XML.CDataVerbatim
                        , XML.cdData = "2017-09-02 21:28:46"
                        })
                  ]
              }
          ]
      }
  expectedDateNoAuthor =
    "<p>Is this thing on?</p>\n\n\n\
    \<h3 id='hakyll-convert-comments-title'>Comments</h3>\n\
    \<div class='hakyll-convert-comment'>\n\
    \<p class='hakyll-convert-comment-date'>On 2017-09-02 21:28:46, unknown author wrote:</p>\n\
    \<div class='hakyll-convert-comment-body'>\n\
    \<p>hi</p>\n\
    \</div>\n\
    \</div>"

  noDateAuthor =
    testCase
      "comments with no \"published\" date but with an author"
      (dpBody (distill True (createInput commentNoDateAuthor)) @?= expectedNoDateAuthor)
  commentNoDateAuthor =
    XML.blank_element
      { XML.elName = commentTag
      , XML.elContent =
          [ XML.Elem $ XML.blank_element
              { XML.elName = commentContentTag
              , XML.elContent =
                  [ XML.Text
                      (XML.blank_cdata
                        { XML.cdVerbatim = XML.CDataVerbatim
                        , XML.cdData = "<p>Here's the thing: …</p>"
                        })
                  ]
              }
          , XML.Elem $ XML.blank_element
              { XML.elName = commentAuthorTag
              , XML.elContent =
                  [ XML.Text
                      (XML.blank_cdata
                        { XML.cdVerbatim = XML.CDataVerbatim
                        , XML.cdData = "Terry Jones"
                        })
                  ]
              }
          ]
      }
  expectedNoDateAuthor =
    "<p>Is this thing on?</p>\n\n\n\
    \<h3 id='hakyll-convert-comments-title'>Comments</h3>\n\
    \<div class='hakyll-convert-comment'>\n\
    \<p class='hakyll-convert-comment-date'>On unknown date, Terry Jones wrote:</p>\n\
    \<div class='hakyll-convert-comment-body'>\n\
    \<p>Here's the thing: …</p>\n\
    \</div>\n\
    \</div>"

  dateAuthor =
    testCase
      "comments with a \"published\" date and an author"
      (dpBody (distill True (createInput commentDateAuthor)) @?= expectedDateAuthor)
  commentDateAuthor =
    XML.blank_element
      { XML.elName = commentTag
      , XML.elContent =
          [ XML.Elem $ XML.blank_element
              { XML.elName = commentContentTag
              , XML.elContent =
                  [ XML.Text
                      (XML.blank_cdata
                        { XML.cdVerbatim = XML.CDataVerbatim
                        , XML.cdData = "<p>It sure is!</p>"
                        })
                  ]
              }
          , XML.Elem $ XML.blank_element
              { XML.elName = commentDateTag
              , XML.elContent =
                  [ XML.Text
                      (XML.blank_cdata
                        { XML.cdVerbatim = XML.CDataVerbatim
                        , XML.cdData = "2017-09-02 21:28:46"
                        })
                  ]
              }
          , XML.Elem $ XML.blank_element
              { XML.elName = commentAuthorTag
              , XML.elContent =
                  [ XML.Text
                      (XML.blank_cdata
                        { XML.cdVerbatim = XML.CDataVerbatim
                        , XML.cdData = "Elizabeth Keyes"
                        })
                  ]
              }
          ]
      }
  expectedDateAuthor =
    "<p>Is this thing on?</p>\n\n\n\
    \<h3 id='hakyll-convert-comments-title'>Comments</h3>\n\
    \<div class='hakyll-convert-comment'>\n\
    \<p class='hakyll-convert-comment-date'>On 2017-09-02 21:28:46, Elizabeth Keyes wrote:</p>\n\
    \<div class='hakyll-convert-comment-body'>\n\
    \<p>It sure is!</p>\n\
    \</div>\n\
    \</div>"

usesTheFirstCommentAuthorTag :: TestTree
usesTheFirstCommentAuthorTag =
  testCase
    "uses the first wp:comment_author tag"
    (dpBody (distill True input) @?= expected)
  where
  input = (RSS.nullItem "Testing...")
    { RSS.rssItemOther =
        [ XML.blank_element
            { XML.elName = contentTag
            , XML.elContent =
                [ XML.Text
                    (XML.blank_cdata
                      { XML.cdVerbatim = XML.CDataVerbatim
                      , XML.cdData = "<p>Check this out!</p>"
                      })
                ]
            }

        , XML.blank_element
            { XML.elName = commentTag
            , XML.elContent =
                [ XML.Elem $ XML.blank_element
                    { XML.elName = commentContentTag
                    , XML.elContent =
                        [ XML.Text
                            (XML.blank_cdata
                              { XML.cdVerbatim = XML.CDataVerbatim
                              , XML.cdData = "<p>Cool!</p>"
                              })
                        ]
                    }
                , XML.Elem $ XML.blank_element
                    { XML.elName = commentAuthorTag
                    , XML.elContent =
                        [ XML.Text
                            (XML.blank_cdata
                              { XML.cdVerbatim = XML.CDataVerbatim
                              , XML.cdData = "Alexander Batischev"
                              })
                        ]
                    }
                , XML.Elem $ XML.blank_element
                    { XML.elName = commentAuthorTag
                    , XML.elContent =
                        [ XML.Text
                            (XML.blank_cdata
                              { XML.cdVerbatim = XML.CDataVerbatim
                              , XML.cdData = "John Doe"
                              })
                        ]
                    }
                ]
            }
        ]
    }

  expected =
    "<p>Check this out!</p>\n\n\n\
    \<h3 id='hakyll-convert-comments-title'>Comments</h3>\n\
    \<div class='hakyll-convert-comment'>\n\
    \<p class='hakyll-convert-comment-date'>On unknown date, Alexander Batischev wrote:</p>\n\
    \<div class='hakyll-convert-comment-body'>\n\
    \<p>Cool!</p>\n\
    \</div>\n\
    \</div>"

turnsIncorrectDatesIntoEpochStart :: TestTree
turnsIncorrectDatesIntoEpochStart =
  testGroup
    "turns incorrect \"published\" dates into Unix epoch start date"
    [ testCase date (dpDate (distill False (createInput date)) @?= expected)
    | date <- [
          "First of April"
        , "2020.07.30"
        , "2020.07.30 00:01"
        , "2020-07-30 00:01"
        , "2020-07-30T00:01"
        , "2020-07-30T00:01Z"
        , "Sun, 31st July, 2020"
        ]
    ]
  where
  createInput date =
    (RSS.nullItem "Testing...")
      { RSS.rssItemPubDate = Just date
      }

  expected = fromGregorian 1970 1 1 0 0 0

parsesDates :: TestTree
parsesDates =
  testGroup
    "parses \"published\" dates"
    [ testCase dateStr (dpDate (distill False (createInput dateStr)) @?= expected)
    | (dateStr, expected) <- [
          ("Sun, 06 Nov 1994 08:49:37 GMT", fromGregorian 1994 11 6 8 49 37)
        , ("Fri, 31 Jul 2020 22:21:59 EST", fromGregorian 2020 8 1 3 21 59)
        ]
    ]
  where
  createInput date =
    (RSS.nullItem "Testing...")
      { RSS.rssItemPubDate = Just date
      }

extractsPostTags :: TestTree
extractsPostTags =
  testCase
    "extracts post's tags"
    (dpTags (distill False input) @?= expected)
  where
  input =
    (RSS.nullItem "Testing tags here")
      { RSS.rssItemCategories =
          [ (RSS.newCategory "first tag") { RSS.rssCategoryDomain = Just "post_tag" }
          , (RSS.newCategory "a non-tag") { RSS.rssCategoryDomain = Just "wrong domain" }
          , (RSS.newCategory "second tag") { RSS.rssCategoryDomain = Just "post_tag" }
          , (RSS.newCategory "another non-tag") { RSS.rssCategoryDomain = Nothing }
          , (RSS.newCategory "third tag") { RSS.rssCategoryDomain = Just "post_tag" }
          ]
      }

  expected = ["first tag", "second tag", "third tag"]

extractsPostCategories :: TestTree
extractsPostCategories =
  testCase
    "extracts post's categories"
    (dpCategories (distill False input) @?= expected)
  where
  input =
    (RSS.nullItem "Testing categories here")
      { RSS.rssItemCategories =
          [ (RSS.newCategory "essays") { RSS.rssCategoryDomain = Just "category" }
          , (RSS.newCategory "a non-category") { RSS.rssCategoryDomain = Just "wrong domain" }
          , (RSS.newCategory "traveling") { RSS.rssCategoryDomain = Just "category" }
          , (RSS.newCategory "another non-category") { RSS.rssCategoryDomain = Nothing }
          ]
      }

  expected = ["essays", "traveling"]
