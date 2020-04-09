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

import Control.Monad.Extra (findM)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Morpheus (interpreter)
import Data.Morpheus.Document (importGQLDocumentWithNamespace)
import Data.Morpheus.Types (GQLRootResolver (..), Undefined (..))
import Data.Text (Text)
import Text.Regex.TDFA ((=~))
import Web.Scotty

importGQLDocumentWithNamespace "schema.graphql"

-- * Library

library :: Monad m => [Author m]
library = [robert, kant, michael]

books :: Monad m => [Book m]
books = [treasure, jekyll, reason, neverending, momo]

-- * Authors

robert :: Monad m => Author m
robert =
  Author
    { authorId = pure 1,
      authorName = pure "Robert Louis Stevenson",
      authorBooks = pure [treasure, jekyll]
    }

kant :: Monad m => Author m
kant =
  Author
    { authorId = pure 2,
      authorName = pure "Immanuel Kant",
      authorBooks = pure [reason]
    }

michael :: Monad m => Author m
michael =
  Author
    { authorId = pure 3,
      authorName = pure "Michael Ende",
      authorBooks = pure [neverending, momo]
    }

-- * Books

treasure :: Monad m => Book m
treasure =
  Book
    { bookId = pure 1,
      bookTitle = pure "Treasure Island",
      bookAuthor = pure robert
    }

jekyll :: Monad m => Book m
jekyll =
  Book
    { bookId = pure 2,
      bookTitle = pure "The Strange Case of Dr Jekyll and Mr Hyde",
      bookAuthor = pure robert
    }

reason :: Monad m => Book m
reason =
  Book
    { bookId = pure 3,
      bookTitle = pure "Critique of Pure Reason",
      bookAuthor = pure kant
    }

neverending :: Monad m => Book m
neverending =
  Book
    { bookId = pure 4,
      bookTitle = pure "The Neverending Story",
      bookAuthor = pure michael
    }

momo :: Monad m => Book m
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
      findM (\Author {authorName} -> (=~ queryAuthorArgsName) <$> authorName) library
    queryAuthors = pure library
    queryBook QueryBookArgs {queryBookArgsTitle} =
      findM (\Book {bookTitle} -> (=~ queryBookArgsTitle) <$> bookTitle) books
    queryBooks = pure books

api :: B.ByteString -> IO B.ByteString
api = interpreter rootResolver

main :: IO ()
main = scotty 8080 $ post "/api" $ raw =<< (liftIO . api =<< body)
