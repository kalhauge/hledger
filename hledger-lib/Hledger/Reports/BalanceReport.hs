{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving #-}
{-|

Balance report, used by the balance command.

-}

{-# LANGUAGE FlexibleInstances, ScopedTypeVariables, OverloadedStrings #-}

module Hledger.Reports.BalanceReport (
  BalanceReport,
  BalanceReportItem,
  balanceReport,
  balanceReportValue,
  mixedAmountValue,
  amountValue,
  flatShowsExclusiveBalance,

  -- * Balances
  Balance (..),
  Total,
  Inclusive (..),
  Exclusive (..),
  BySpan (..),

  -- * Tests
  tests_Hledger_Reports_BalanceReport
)
where

import Data.List as List
import Data.Monoid
import Data.Ord
import Data.Time.Calendar
import qualified Data.Map as Map
import qualified Data.Text as T

import Hledger.Data
import Hledger.Data.Trie as Trie
import Hledger.Data.Span as Span
import Hledger.Data.MonoidalMap as MM
import Hledger.Query
import Hledger.Read (mamountp')
import Hledger.Reports.ReportOptions
import Hledger.Utils

import Test.HUnit

account'FromName = accountNameComponents

class Monoid bal => Balance bal where
  fromPosting :: Transaction -> Posting -> bal

  journalBalance :: Query -> Journal -> bal
  journalBalance q j =
    foldMap (uncurry fromPosting) $ journalQueryPostings q j

-- | A simple single-column balance report. It has:
--
-- 1. a list of items, one per account, each containing:
--
--   * the full account name
--
--   * the Ledger-style elided short account name
--     (the leaf account name, prefixed by any boring parents immediately above);
--     or with --flat, the full account name again
--
--   * the number of indentation steps for rendering a Ledger-style account tree,
--     taking into account elided boring parents, --no-elide and --flat
--
--   * an amount
--
-- 2. the total of all amounts
--
type BalanceReport = ([BalanceReportItem], MixedAmount)
type BalanceReportItem = (AccountName, AccountName, Int, MixedAmount)

-- | When true (the default), this makes balance --flat reports and their implementation clearer.
-- Single/multi-col balance reports currently aren't all correct if this is false.
flatShowsExclusiveBalance    = True

balanceReport :: ReportOpts -> Query -> Journal -> BalanceReport
balanceReport opts q j
  | flat_ opts = flatReport
  | otherwise = treeReport
  where
    flatReport = (items, total)
      where
        Exclusive balance = journalBalance q j :: Exclusive Total
        total = getSum . Map.foldMapWithKey (\_ s -> s) . unMonoidalMap $ balance
        items =
          map (\(name, total) -> (aname' name, aname' name, 0, getSum total))
          . Map.toAscList
          . unMonoidalMap
          $ balance

    treeReport =  {-# SCC "treeReport" #-} snd $ Trie.reduce reductor balance
      where
        Inclusive balance =
          journalBalance q j :: Inclusive Total

        reductor
          :: Sum MixedAmount
          -> [(T.Text, (Account', BalanceReport))]
          -> (Account', BalanceReport)
        reductor (Sum total) subs =
          case subs of
            -- An item is boring if it only has one sub,
            -- if that is the case promote that sub to be the active node.
            [(name, (an, (items', total')))]
              | total' == total ->
              (name:an, (map (addName name) items', total))
            _ ->
              -- If more or zero subs combine them, an choose the empty
              -- name space
              ([], (concatMap (uncurry combine) subs, total))

        combine name (an, (items', total'))
          | isZeroMixedAmount total' && null items' && not (empty_ opts) = []
          | otherwise =
              (aname' (name:an), aname' (name:an), 0, total')
              : map (addName name) items'

        addName name (an, elied, i, ma) =
          (aname' [name, an], elied, i + 1, ma)

    aname' = accountNameFromComponents

-- -- | Enabling this makes balance --flat --empty also show parent accounts without postings,
-- -- in addition to those with postings and a zero balance. Disabling it shows only the latter.
-- -- No longer supported, but leave this here for a bit.
--   -- flatShowsPostinglessAccounts = True

-- | Convert all the amounts in a single-column balance report to
-- their value on the given date in their default valuation
-- commodities.
balanceReportValue :: Journal -> Day -> BalanceReport -> BalanceReport
balanceReportValue j d r = r'
  where
    (items,total) = r
    r' =
      dbg8 "known market prices" (jmarketprices j) `seq`
      dbg8 "report end date" d `seq`
      dbg8 "balanceReportValue"
        ([(n, n', i, mixedAmountValue j d a) |(n,n',i,a) <- items], mixedAmountValue j d total)

mixedAmountValue :: Journal -> Day -> MixedAmount -> MixedAmount
mixedAmountValue j d (Mixed as) = Mixed $ map (amountValue j d) as

-- | Find the market value of this amount on the given date, in it's
-- default valuation commodity, based on recorded market prices.
-- If no default valuation commodity can be found, the amount is left
-- unchanged.
amountValue :: Journal -> Day -> Amount -> Amount
amountValue j d a =
  case commodityValue j d (acommodity a) of
    Just v  -> v{aquantity=aquantity v * aquantity a
                ,aprice=aprice a
                }
    Nothing -> a

-- | Find the market value, if known, of one unit of this commodity (A) on
-- the given valuation date, in the commodity (B) mentioned in the latest
-- applicable market price. The latest applicable market price is the market
-- price directive for commodity A with the latest date that is on or before
-- the valuation date; or if there are multiple such prices with the same date,
-- the last parsed.
commodityValue :: Journal -> Day -> CommoditySymbol -> Maybe Amount
commodityValue j valuationdate c
    | null applicableprices = dbg Nothing
    | otherwise             = dbg $ Just $ mpamount $ last applicableprices
  where
    dbg = dbg8 ("using market price for "++T.unpack c)
    applicableprices =
      [p | p <- sortBy (comparing mpdate) $ jmarketprices j
      , mpcommodity p == c
      , mpdate p <= valuationdate
      ]

-- | Total is a Sum of 'MixedAmount'
type Total = Sum MixedAmount

instance Balance Total where
  fromPosting _ = Sum . pamount

-- | Inclusive is a grouping of balances over Accounts
newtype Inclusive bal =
  Inclusive (Trie AccountName bal)
  deriving (Show, Eq, Monoid)

instance Balance bal => Balance (Inclusive bal) where
  fromPosting t p = Inclusive $ Trie.singleton name $ fromPosting t p
    where name = account'FromName $ paccount p

-- | Exclusive is a grouping of balances over 'Account', totals of
-- super accounts are exclusive of .
newtype Exclusive bal =
  Exclusive (MonoidalMap Account' bal)
  deriving (Show, Eq, Monoid)

instance Balance bal => Balance (Exclusive bal) where
  fromPosting t p = Exclusive $ MM.singleton name $ fromPosting t p
    where name = account'FromName $ paccount p

-- | Group a balance over spans
newtype BySpan bl =
  BySpan (MonoidalMap (Span Day) bl)
  deriving (Show, Eq, Monoid)

instance Balance bl => Balance (BucketTree Day -> BySpan bl) where
  fromPosting t p bd =
    BySpan $ MM.singleton (Span.lookup date bd) $ fromPosting t p
    where date = maybe (tdate t) id $ pdate p

tests_balanceReport =
  let
    (opts,journal) `gives` r = do
      let (eitems, etotal) = r
          (aitems, atotal) = balanceReport opts (queryFromOpts nulldate opts) journal
          showw (acct,acct',indent,amt) =
            -- (acct, acct', indent)
            (acct, acct', indent, showMixedAmountDebug amt)
      assertEqual "items" (map showw eitems) (map showw aitems)
      assertEqual "total" (showMixedAmountDebug etotal) (showMixedAmountDebug atotal)
    usd0 = usd 0
  in [

   "balanceReport with no args on null journal" ~: do
   (defreportopts, nulljournal) `gives` ([], Mixed [nullamt])

  ,"balanceReport with no args on sample journal" ~: do
   (defreportopts, samplejournal) `gives`
    ([
      ("assets","assets",0, mamountp' "$0.00")
     ,("assets:bank","bank",1, mamountp' "$2.00")
     ,("assets:bank:checking","checking",2, mamountp' "$1.00")
     ,("assets:bank:saving","saving",2, mamountp' "$1.00")
     ,("assets:cash","cash",1, mamountp' "$-2.00")
     ,("expenses","expenses",0, mamountp' "$2.00")
     ,("expenses:food","food",1, mamountp' "$1.00")
     ,("expenses:supplies","supplies",1, mamountp' "$1.00")
     ,("income","income",0, mamountp' "$-2.00")
     ,("income:gifts","gifts",1, mamountp' "$-1.00")
     ,("income:salary","salary",1, mamountp' "$-1.00")
     ],
     Mixed [usd0])

  ,"balanceReport with --depth=N" ~: do
   (defreportopts{depth_=Just 1}, samplejournal) `gives`
    ([
     ("expenses",    "expenses",    0, mamountp'  "$2.00")
     ,("income",      "income",      0, mamountp' "$-2.00")
     ],
     Mixed [usd0])

  ,"balanceReport with depth:N" ~: do
   (defreportopts{query_="depth:1"}, samplejournal) `gives`
    ([
     ("expenses",    "expenses",    0, mamountp'  "$2.00")
     ,("income",      "income",      0, mamountp' "$-2.00")
     ],
     Mixed [usd0])

  ,"balanceReport with a date or secondary date span" ~: do
   (defreportopts{query_="date:'in 2009'"}, samplejournal2) `gives`
    ([],
     Mixed [nullamt])
   (defreportopts{query_="date2:'in 2009'"}, samplejournal2) `gives`
    ([
      ("assets:bank:checking","assets:bank:checking",0,mamountp' "$1.00")
     ,("income:salary","income:salary",0,mamountp' "$-1.00")
     ],
     Mixed [usd0])

  ,"balanceReport with desc:" ~: do
   (defreportopts{query_="desc:income"}, samplejournal) `gives`
    ([
      ("assets:bank:checking","assets:bank:checking",0,mamountp' "$1.00")
     ,("income:salary","income:salary",0, mamountp' "$-1.00")
     ],
     Mixed [usd0])

  ,"balanceReport with not:desc:" ~: do
   (defreportopts{query_="not:desc:income"}, samplejournal) `gives`
    ([
      ("assets","assets",0, mamountp' "$-1.00")
     ,("assets:bank:saving","bank:saving",1, mamountp' "$1.00")
     ,("assets:cash","cash",1, mamountp' "$-2.00")
     ,("expenses","expenses",0, mamountp' "$2.00")
     ,("expenses:food","food",1, mamountp' "$1.00")
     ,("expenses:supplies","supplies",1, mamountp' "$1.00")
     ,("income:gifts","income:gifts",0, mamountp' "$-1.00")
     ],
     Mixed [usd0])

  ,"balanceReport with period on a populated period" ~: do
    (defreportopts{period_= PeriodBetween (fromGregorian 2008 1 1) (fromGregorian 2008 1 2)}, samplejournal) `gives`
     (
      [
       ("assets:bank:checking","assets:bank:checking",0, mamountp' "$1.00")
      ,("income:salary","income:salary",0, mamountp' "$-1.00")
      ],
      Mixed [usd0])

   ,"balanceReport with period on an unpopulated period" ~: do
    (defreportopts{period_= PeriodBetween (fromGregorian 2008 1 2) (fromGregorian 2008 1 3)}, samplejournal) `gives`
     ([],Mixed [nullamt])



{-
    ,"accounts report with account pattern o" ~:
     defreportopts{patterns_=["o"]} `gives`
     ["                  $1  expenses:food"
     ,"                 $-2  income"
     ,"                 $-1    gifts"
     ,"                 $-1    salary"
     ,"--------------------"
     ,"                 $-1"
     ]

    ,"accounts report with account pattern o and --depth 1" ~:
     defreportopts{patterns_=["o"],depth_=Just 1} `gives`
     ["                  $1  expenses"
     ,"                 $-2  income"
     ,"--------------------"
     ,"                 $-1"
     ]

    ,"accounts report with account pattern a" ~:
     defreportopts{patterns_=["a"]} `gives`
     ["                 $-1  assets"
     ,"                  $1    bank:saving"
     ,"                 $-2    cash"
     ,"                 $-1  income:salary"
     ,"                  $1  liabilities:debts"
     ,"--------------------"
     ,"                 $-1"
     ]

    ,"accounts report with account pattern e" ~:
     defreportopts{patterns_=["e"]} `gives`
     ["                 $-1  assets"
     ,"                  $1    bank:saving"
     ,"                 $-2    cash"
     ,"                  $2  expenses"
     ,"                  $1    food"
     ,"                  $1    supplies"
     ,"                 $-2  income"
     ,"                 $-1    gifts"
     ,"                 $-1    salary"
     ,"                  $1  liabilities:debts"
     ,"--------------------"
     ,"                   0"
     ]

    ,"accounts report with unmatched parent of two matched subaccounts" ~:
     defreportopts{patterns_=["cash","saving"]} `gives`
     ["                 $-1  assets"
     ,"                  $1    bank:saving"
     ,"                 $-2    cash"
     ,"--------------------"
     ,"                 $-1"
     ]

    ,"accounts report with multi-part account name" ~:
     defreportopts{patterns_=["expenses:food"]} `gives`
     ["                  $1  expenses:food"
     ,"--------------------"
     ,"                  $1"
     ]

    ,"accounts report with negative account pattern" ~:
     defreportopts{patterns_=["not:assets"]} `gives`
     ["                  $2  expenses"
     ,"                  $1    food"
     ,"                  $1    supplies"
     ,"                 $-2  income"
     ,"                 $-1    gifts"
     ,"                 $-1    salary"
     ,"                  $1  liabilities:debts"
     ,"--------------------"
     ,"                  $1"
     ]

    ,"accounts report negative account pattern always matches full name" ~:
     defreportopts{patterns_=["not:e"]} `gives`
     ["--------------------"
     ,"                   0"
     ]

    ,"accounts report negative patterns affect totals" ~:
     defreportopts{patterns_=["expenses","not:food"]} `gives`
     ["                  $1  expenses:supplies"
     ,"--------------------"
     ,"                  $1"
     ]

    ,"accounts report with -E shows zero-balance accounts" ~:
     defreportopts{patterns_=["assets"],empty_=True} `gives`
     ["                 $-1  assets"
     ,"                  $1    bank"
     ,"                   0      checking"
     ,"                  $1      saving"
     ,"                 $-2    cash"
     ,"--------------------"
     ,"                 $-1"
     ]

    ,"accounts report with cost basis" ~: do
       j <- (readJournal Nothing Nothing Nothing $ unlines
              [""
              ,"2008/1/1 test           "
              ,"  a:b          10h @ $50"
              ,"  c:d                   "
              ]) >>= either error' return
       let j' = journalCanonicaliseAmounts $ journalConvertAmountsToCost j -- enable cost basis adjustment
       balanceReportAsText defreportopts (balanceReport defreportopts Any j') `is`
         ["                $500  a:b"
         ,"               $-500  c:d"
         ,"--------------------"
         ,"                   0"
         ]
-}
 ]

Right samplejournal2 =
  journalBalanceTransactions False
    nulljournal{
      jtxns = [
        txnTieKnot Transaction{
          tindex=0,
          tsourcepos=nullsourcepos,
          tdate=parsedate "2008/01/01",
          tdate2=Just $ parsedate "2009/01/01",
          tstatus=Unmarked,
          tcode="",
          tdescription="income",
          tcomment="",
          ttags=[],
          tpostings=
            [posting {paccount="assets:bank:checking", pamount=Mixed [usd 1]}
            ,posting {paccount="income:salary", pamount=missingmixedamt}
            ],
          tpreceding_comment_lines=""
        }
      ]
    }

-- tests_isInterestingIndented = [
--   "isInterestingIndented" ~: do
--    let (opts, journal, acctname) `gives` r = isInterestingIndented opts l acctname `is` r
--           where l = ledgerFromJournal (queryFromOpts nulldate opts) journal

--    (defreportopts, samplejournal, "expenses") `gives` True
--  ]

tests_Hledger_Reports_BalanceReport :: Test
tests_Hledger_Reports_BalanceReport = TestList
  tests_balanceReport
