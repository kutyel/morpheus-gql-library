{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.List (find)
import Data.Morpheus (interpreter)
import Data.Morpheus.Document (importGQLDocumentWithNamespace)
import Data.Morpheus.Types (GQLRootResolver (..), Undefined (..))
import Data.Text (Text)
import Web.Scotty

importGQLDocumentWithNamespace "schema.graphql"

-- * Library

library :: Applicative m => [Author m]
library = [robert, kant, michael]

-- * Authors

robert :: Applicative m => Author m
robert =
  Author
    { authorId = pure 1,
      authorName = pure "Robert Louis Stevenson",
      authorBooks = pure [treasure, jekyll]
    }

kant :: Applicative m => Author m
kant =
  Author
    { authorId = pure 2,
      authorName = pure "Immanuel Kant",
      authorBooks = pure [reason]
    }

michael :: Applicative m => Author m
michael =
  Author
    { authorId = pure 3,
      authorName = pure "Michael Ende",
      authorBooks = pure [neverending, momo]
    }

-- * Books

treasure :: Applicative m => Book m
treasure =
  Book
    { bookId = pure 1,
      bookTitle = pure "Treasure Island",
      bookAuthor = pure robert
    }

jekyll :: Applicative m => Book m
jekyll =
  Book
    { bookId = pure 2,
      bookTitle = pure "The Strange Case of Dr Jekyll and Mr Hyde",
      bookAuthor = pure robert
    }

reason :: Applicative m => Book m
reason =
  Book
    { bookId = pure 3,
      bookTitle = pure "Critique of Pure Reason",
      bookAuthor = pure kant
    }

neverending :: Applicative m => Book m
neverending =
  Book
    { bookId = pure 4,
      bookTitle = pure "The Neverending Story",
      bookAuthor = pure michael
    }

momo :: Applicative m => Book m
momo =
  Book
    { bookId = pure 5,
      bookTitle = pure "Momo",
      bookAuthor = pure michael
    }

-- * GraphQL Resolvers

rootResolver :: GQLRootResolver IO () Query Undefined Undefined
rootResolver =
  GQLRootResolver
    { queryResolver =
        Query
          { queryAuthor,
            queryAuthors,
            queryBook,
            queryBooks
          },
      mutationResolver = Undefined,
      subscriptionResolver = Undefined
    }
  where
    queryAuthor QueryAuthorArgs {queryAuthorArgsName} =
      pure $ Just robert -- TODO: use find
    queryAuthors =
      pure library
    queryBook QueryBookArgs {queryBookArgsTitle} =
      pure $ Just treasure -- TODO: use find
    queryBooks =
      pure [treasure, jekyll, reason, neverending, momo]

api :: B.ByteString -> IO B.ByteString
api = interpreter rootResolver

main :: IO ()
main = scotty 8080 $ post "/api" $ raw =<< (liftIO . api =<< body)
