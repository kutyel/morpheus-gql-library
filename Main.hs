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
import Data.Morpheus (interpreter)
import Data.Morpheus.Document (importGQLDocumentWithNamespace)
import Data.Morpheus.Types (GQLRootResolver (..), IORes, Undefined (..))
import Data.Text (Text)
import Web.Scotty

importGQLDocumentWithNamespace "schema.graphql"

-- Library

momo :: Applicative m => Book m
momo =
  Book
    { bookId = pure 5,
      bookTitle = pure "Momo",
      bookAuthor = pure michael
    }

michael :: Applicative m => Author m
michael =
  Author
    { authorId = pure 1,
      authorName = pure "Michael Ende",
      authorBooks = pure [momo]
    }

-- GraphQL Resolvers

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
    queryAuthor QueryAuthorArgs {queryAuthorArgsName} = pure $ Just michael
    queryAuthors = undefined
    queryBook = undefined
    queryBooks = undefined

api :: B.ByteString -> IO B.ByteString
api = interpreter rootResolver

main :: IO ()
main = scotty 8080 $ post "/api" $ raw =<< (liftIO . api =<< body)
