module HttpExamples where

import Data.ByteString.Lazy
import Network.Wreq

urls :: [String]
urls = [ "http://httpbin.org/ip"
      ,  "https://jsonplaceholder.typicode.com/posts/2"
      ]

mappingGet :: [IO (Response ByteString)]
mappingGet = map get urls

traversedUrls :: IO [Reponse ByteString]
traversedUrls = traverse get urls


