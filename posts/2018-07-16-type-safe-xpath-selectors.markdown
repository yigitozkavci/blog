---
title: Type safe xpath selectors by generating compile-time constraints
author: Yiğit Özkavcı
---

In [Picus Security](http://picussecurity.com/), we use the [webdriver](https://hackage.haskell.org/package/webdriver) library extensively for our Selenium end-to-end tests. The library itself is very convenient in terms of user API, but when it comes to actually debugging end to end test code, things aren't quite smooth compared to just writing the tests.

The fact that Selenium needs to be told a timeout for failure cases makes it really hard to debug end to end tests, because we just need to stare at the screen for 1 minute / 5 minutes and wait for Selenium to show us the result of the test. This arises the question: can we actually make sure that our **XPaths** and **actions** make sense?

I've been working on a more strict version of [webdriver](https://hackage.haskell.org/package/webdriver), [typed-webdriver](https://github.com/yigitozkavci/typed-webdriver). It leverages TemplateHaskell to convert your xpath strings to well-kinded versions with associated capabilities, then it carries this information with constraints to other functions. This is quite confusing, so let's jump to an example:

```haskell
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}

import Test.TWebDriver.Commands (Capability (..), Selector (..), WD,
                                 click, findElem, mkXPath)

-- 1) Typechecks, because we try to click to a element of type link.
myS :: WD ()
myS = click =<< myFindElem $(mkXPath "//span/button/descendant::a[contains(text(), 'wow')]")
```

```haskell
-- 2) Does not compile, because we try to click to a span element, which should not be clickable.
myS :: WD ()
myS = click =<< myFindElem $(mkXPath "//span/button/descendant::span[contains(text(), 'wow')]")
{-
  TypeError:
  The following XPath is not clickable:
  //a/button/descendant::span[contains(text(), 'wow')]
-}
```

If you are unfamiliar to XPath, here is what is happening:

  - In case 1), we target an element like `<span><button><a>wowo</a></button></span>`. Since an element with tag `a` is clickable, we allow calling `click` on **elements found by this xpath**.

  - In the case 2), however, we are targeting a `span` character and we very well know that we are not supposed to click on elements of tag `span`, hence we reject this code in compile-time. But... how?

In order to reject a code in compile-time, we need to somehow make constraints and type-level assertions. One way to do this is to use a type-list of capabilities that a XPath selector has.

An example of what we produce in a call to `mkXPath`:
```haskell
> :set -XTemplateHaskell
> :set -XDataKinds
> import Language.Haskell.TH
>
> :t $(mkXPath "//span/button/descendant::a[contains(text(), 'wow')]")
$(mkXPath "//span/button/descendant::a[contains(text(), 'wow')]")
  :: Selector
       "//span/button/descendant::a[contains(text(), 'wow')]"
       '['Clickable, 'ContainsText]

```

TemplateHaskell has its own constructors for Haskell types, values, functions, instances etc. And it's very hard to try to memorize every construction, so we simply use the repl and type:
```
> runQ [e|Selector :: Selector "yigit"|]
SigE
  (ConE Test.TWebDriver.Commands.Selector)
  (AppT (ConT Test.TWebDriver.Commands.Selector) (LitT (StrTyLit "yigit")))
```
We cannot use this quasi quotation everywhere, but it still helps us understand how TemplateHaskell constructs values.

And here is the definition of `mkXPath`:
```haskell
mkXPath :: String -> Q Exp
mkXPath name = do
  promotedTySig <- toPromotedTH (generateCapabilities name)
  pure $
    SigE
      (ConE (mkName "Selector"))
      (AppT
        (AppT
          (ConT (mkName "Selector"))
          (LitT (StrTyLit name))
        )
        promotedTySig
      )

toPromotedTH :: Either String [Capability] -> Q Type
toPromotedTH (Left err) = reportError err $> PromotedNilT
toPromotedTH (Right []) = pure PromotedNilT
toPromotedTH (Right (cap:xs)) = do
  deeper <- toPromotedTH (Right xs)
  pure $
    AppT
      (AppT
        PromotedConsT
        (PromotedT (mkName (show cap)))
      )
      deeper
```
So we basically create a type-level string for consuming xpath in "sending to Selenium" process, and also a list of capabilities this XPath has.

After creating our qualified selector, we need to be able to find elements with it:

```haskell
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

import qualified Test.WebDriver.Commands    as WDM
import qualified Test.WebDriver.Monad       as WDM
import           GHC.TypeLits               (KnownSymbol, Symbol, symbolVal)
import qualified Data.Text                  as T

findElem
  :: forall name cap. KnownSymbol name
  => Selector name cap
  -> WDM.WD (Element name cap)
findElem Selector = fmap Element
                  $ WDM.findElem
                  $ WDM.ByXPath
                  $ T.pack
                  $ symbolVal (Proxy :: Proxy name)
```

The capabilities we have at type level are used by several library functions. For instance, let's take a look at the signature of click function:

```haskell
data Capability =
    Clickable
  | Hoverable
  | ContainsText
  deriving Show

type family XPathNotClickableError (name :: Symbol) :: b where
  XPathNotClickableError name =
    TypeError ( 'Text "The following XPath is not clickable: "
          ':$$: 'Text name
    )

type family CanClick (name :: Symbol) (xs :: [Capability]) :: Constraint where
  CanClick name '[] = XPathNotClickableError name
  CanClick _ ('Clickable ': xs) = ()
  CanClick name (x ': xs) = CanClick name xs

click :: forall name xs. CanClick name xs => Element name xs -> WDM.WD ()
click = WDM.click . unElement

```
This way, after encoding necessary capabilities in a Selector's type, we can observe what actions can be taken with this selector thgouhgout the journey of compilation.

This library is still in being developed, and not meant to be used in production yet.

The purpose of this blog post is to demonstrate how we can leverage lightweight usage of TemplateHaskell to generate compile-time constraints for us via regular Haskell expressions.

Cheers.
