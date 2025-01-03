{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Data.Maybe                           (fromMaybe)
import           Network.Wai.Middleware.RequestLogger (logStdout)
import           Network.Wai.Middleware.Static        (static)
import           System.Environment                   (lookupEnv)
import           Text.Blaze.Html                      (Html)
import           Text.Blaze.Html.Renderer.Text        (renderHtml)
import qualified Text.Blaze.Html5                     as H
import           Text.Blaze.Html5                     ((!))
import qualified Text.Blaze.Html5.Attributes          as A
import           Web.Scotty

viewHead :: String -> Html
viewHead title = H.html ! A.lang "en" $
  H.head $ do
    H.link ! A.rel "stylesheet" ! A.href "/styles.css"
    H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
    H.meta ! A.name "description" ! A.content (mconcat
        [ "Alexander is a software developer from Ecuador. He is interested in and writes regularly about "
        , "functional programming and distributed systems in his blog. In his free time, he likes doing "
        , "street photography, reading and working on side-projects. If you share his love for FP and Haskell "
        , "or programming in general, don't hesitate to reach out to him."
        ])
    H.meta ! A.name "author" ! A.content "Alexander Goussas"
    H.meta ! A.name "keywords" ! A.content "Haskell, Blog, Programming"
    H.title $ H.toHtml title

viewHome :: Html
viewHome = viewHead "Home | Alexander Goussas" >> viewAppBar >> H.main (do
  H.h1 "Alexander Goussas"
  H.img ! A.alt "Haskell programmer" ! A.src "/me.jpg"
  H.p $ mconcat
    [ "Hello there! I am Alexander, a programmer from the city of Guayaquil, Ecuador. "
    , "I am currently finishing my Computer Engineering degree while also working as a software developer "
    , "at a respectable software company TM."
    ]
  H.p $ mconcat 
    [ "Among other things, I love photography, tattoos and learning foreign languages. "
    , "I do street photography on my free time and I'm currently learning French. "
    ]
  H.p $ mconcat
    ["Lastly, I run an online programming book club (see below if you are interested) and love talking all things programming,"
    ," so don't hesitate to reach out if you want to chat!"]
  viewInterestingStuff
  viewSocials)

viewBookClub :: Html
viewBookClub = viewHead "Book Club | Alexander Goussas" >> viewAppBar >> H.main (do
  H.h1 "GYE Systems Programming Book Club"
  H.div ! A.id "sign-up" $ do
    "Join "
    H.a ! A.href "https://forms.gle/kTYk5kc1Y4sv3MXq9" $ "here"
    ", or join "
    H.a ! A.href "https://discord.gg/kbK45MDk7q" $ "the Discord server."
  H.div "SPBC is a book club where members discuss programming books through email and engage in exciting and provocative conversation."
  H.div $ mconcat
    [ "The criteria for picking books is very loose, anything related to systems programming is fine: programming language theory, "
    , "database design and internals, operating systems, software architecture; etc. We should be able to finish the book in 1-3 "
    , "months reading 1-2 chapters per week."
    ]
  H.div $ mconcat
    [ "After reading each chapter, a member of the group would start the conversation about it, purely via email. Others can then follow. "
    , "Conversation can be either in English or Spanish, whatever the participants feel more comfortable with."
    ]
  H.div "It might be possible to have in-person meetings from time to time if the members of the group want to.")

viewTheCurryCorner :: Html
viewTheCurryCorner = viewHead "The Curry Corner | Alexander Goussas" >> viewAppBar >> H.main (do
  H.h1 "The Curry Corner"
  H.div "Welcome to The Curry Corner!"
  H.div "This is a series of videos talking about different topics relating to functional programming."
  H.div $ mconcat
    [ "I am currently looking for a co-host to run the show. The dynamic I am going for is: hosts alternate episodes "
    , "between the driver and copilot seat. The driver runs the episode, and the copilot helps with the live chat and pointing "
    , "things out or correcting the driver if necessary."
    ]
  H.div "To give you an idea of what I am going for, some of the topics I plan on treating are: "
  H.ul $ do
    H.li "Translating common algorithms to use recursion"
    H.li "Tagless final with fp-ts and comparison with Haskell/Scala"
    H.li "Stateful computation in functional languages"
    H.li "Property based testing"
  H.div "Among others. If this is something that interests you, please don't hesitate to reach out to me.")

viewAppBar :: Html
viewAppBar = H.header $ H.nav $
  H.ul $ do
    H.li $ H.a ! A.href "/" $ "Home"
    H.li $ H.a ! A.href "https://aloussase.substack.com" $ "Blog"
    H.li $ H.a ! A.href "/spbc" $ "Book Club"
    H.li $ H.a ! A.href "/curry-corner" $ "The Curry Corner"

viewInterestingStuff :: Html
viewInterestingStuff = do
  H.h2 "Interesting stuff"
  H.div "Some of this stuff I run, others I am a small part of"
  H.ul $ do
    H.li $ do
      "My blog "
      H.a ! A.href "https://aloussase.substack.com" $ "Ramblings of the One-Eyed Daemon"
      " where I talk mostly about web stuff, distributed systems and Haskell"
    H.li $ do
      "The online book club I run "
      H.a ! A.href "/spbc" $ "GYE Systems Programming Book Club"
    H.li $ do
      "The Discord server I run "
      H.a ! A.href "https://discord.gg/kbK45MDk7q" $ "Invite link"
    H.li $ do
      "Co-hosted YouTube channel talking all things functional programming "
      H.a ! A.href "/curry-corner" $ "The Curry Corner"
    H.li $ do
      "Free Software university club (I was president for a couple years) "
      H.a ! A.href "https://kokoa.espol.edu.ec" $ "Kokoa"

viewSocials :: Html
viewSocials = do
  H.h2 "Socials"
  H.div "I don't really use social media. The best way to reach me is either by email or LinkedIn"
  H.ul $ do
    H.li $ H.a ! A.href "https://linkedin.com/in/alexander-goussas" $ "LinkedIn"
    H.li $ H.a ! A.href "https://github.com/aloussase" $ "GitHub"
    H.li $ H.a ! A.href "mailto:goussasalexander@gmail.com" $ "Email"


app :: Int -> IO ()
app port = scotty port $ do
  middleware static
  middleware logStdout

  get "/" $ html . renderHtml $ viewHome
  get "/spbc" $ html . renderHtml $ viewBookClub
  get "/curry-corner" $ html . renderHtml $ viewTheCurryCorner


main :: IO ()
main = do
  port <- fmap (fmap read) (lookupEnv "PORT")
  app (fromMaybe 3000 port)
