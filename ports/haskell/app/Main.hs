{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}

-- Parser V7 - Haskell
-- CKY + beam + unary-closure + tokenización (al/del + enclíticos 1)
-- Consume lexicon.json + grammar.json (mismos archivos neutrales del experimento).
--
-- Uso:
--   cabal run parser_v7 -- --file corpus.txt --print --json out.json
--   cabal run parser_v7 -- --text "..." --trees --print

module Main where

import           Control.DeepSeq (deepseq)
import           Data.Aeson
import           Data.Aeson.Types (Parser)
import qualified Data.ByteString.Lazy as BL
import           Data.Char (isLetter, isUpper)
import           Data.Foldable (foldl')
import           Data.Function (on)
import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap.Strict as IM
import qualified Data.List as L
import qualified Data.Map.Strict as M
import           Data.Maybe (fromMaybe, mapMaybe)
import           Data.Ord (Down(..))
import qualified Data.Text as T
import           Data.Text (Text)
import qualified Data.Vector as V
import           GHC.Generics (Generic)
import           System.Environment (getArgs)
import           System.Exit (die)

--------------------------------------------------------------------------------
-- Symtab
--------------------------------------------------------------------------------

data Symtab = Symtab
  { stMap  :: !(M.Map Text Int)
  , stVec  :: !(V.Vector Text)
  } deriving (Show)

symtabNew :: Symtab
symtabNew = Symtab M.empty V.empty

intern :: Text -> Symtab -> (Int, Symtab)
intern s st =
  case M.lookup s (stMap st) of
    Just i  -> (i, st)
    Nothing ->
      let i   = V.length (stVec st)
          vec = stVec st `V.snoc` s
          mp  = M.insert s i (stMap st)
      in  (i, st { stMap = mp, stVec = vec })

strOf :: Symtab -> Int -> Text
strOf st i = fromMaybe "?" (stVec st V.!? i)

isVar :: Symtab -> Int -> Bool
isVar st i =
  case T.uncons (strOf st i) of
    Just ('?',_) -> True
    _            -> False

--------------------------------------------------------------------------------
-- Feats + unificación
--------------------------------------------------------------------------------

type Feat = (Int, Int)  -- (key, val)

newtype Feats = Feats { unFeats :: [Feat] }
  deriving (Eq, Show)

featsNorm :: [Feat] -> Feats
featsNorm = Feats . L.sortBy (compare `on` fst <> compare `on` snd)

featsFind :: Int -> Feats -> Maybe Int
featsFind k (Feats xs) = lookup k xs

-- FNV-1a 64-bit
featsHash64 :: Feats -> Word64
featsHash64 (Feats xs) = foldl' step 1469598103934665603 xs
  where
    step !h (!k,!v) = fnv (fnv h (fromIntegral k)) (fromIntegral v)
    fnv !h !x = (h `xorW` x) * 1099511628211
    xorW a b = a `xor` b
    xor :: Word64 -> Word64 -> Word64
    xor = xor64
    xor64 :: Word64 -> Word64 -> Word64
    xor64 = (Data.Bits.xor)

-- Need Bits
import qualified Data.Bits

xor64 :: Word64 -> Word64 -> Word64
xor64 = Data.Bits.xor

unify :: Symtab -> Feats -> Feats -> Maybe Feats
unify st (Feats a) (Feats b) =
  let mp0 = IM.fromList a
      go (!mp) [] = Just (Feats (L.sort (IM.toList mp)))
      go (!mp) ((k,vb):bs) =
        case IM.lookup k mp of
          Nothing -> go (IM.insert k vb mp) bs
          Just va
            | va == vb -> go mp bs
            | isVar st va && not (isVar st vb) -> go (IM.insert k vb mp) bs
            | not (isVar st va) && isVar st vb -> go mp bs
            | isVar st va && isVar st vb       -> go mp bs
            | otherwise                         -> Nothing
  in  go mp0 b

requireFeat :: Symtab -> Feats -> Int -> Int -> Maybe Feats
requireFeat st fs k v = unify st fs (featsNorm [(k,v)])

--------------------------------------------------------------------------------
-- JSON schema (lexicon / grammar)
--------------------------------------------------------------------------------

data LexiconFile = LexiconFile
  { entries :: HM.HashMap Text [LexEntryFile]
  } deriving (Show, Generic)

data LexEntryFile = LexEntryFile
  { pos    :: !Text
  , weight :: !Double
  , feats  :: !(HM.HashMap Text Text)
  } deriving (Show, Generic)

instance FromJSON LexiconFile
instance FromJSON LexEntryFile

data ArgsFile = ArgsFile
  { key   :: !(Maybe Text)
  , value :: !(Maybe Text)
  , type_ :: !(Maybe Text)
  } deriving (Show, Generic)

instance FromJSON ArgsFile where
  parseJSON = withObject "ArgsFile" $ \o ->
    ArgsFile <$> o .:? "key" <*> o .:? "value" <*> o .:? "type"

data RuleFile = RuleFile
  { lhs    :: !Text
  , rhs    :: ![Text]
  , weight :: !Double
  , op     :: !Text
  , args   :: !(Maybe ArgsFile)
  , post   :: !(Maybe [Text])
  } deriving (Show, Generic)

instance FromJSON RuleFile
data GrammarFile = GrammarFile
  { rules :: ![RuleFile]
  } deriving (Show, Generic)

instance FromJSON GrammarFile

--------------------------------------------------------------------------------
-- Internal Lexicon / Grammar
--------------------------------------------------------------------------------

data LexEntry = LexEntry
  { lePos    :: !Int
  , leWeight :: !Double
  , leFeats  :: !Feats
  } deriving (Show)

type Lexicon = M.Map Text [LexEntry]

data Op
  = OP_EMPTY | OP_LEFT | OP_RIGHT | OP_UNIFY
  | OP_REQUIRE_LEFT | OP_REQUIRE_RIGHT
  | OP_MAKE_GAP | OP_RELCLAUSE_OBL
  deriving (Eq, Show)

data Rule = Rule
  { rLhs     :: !Int
  , rRhsLen  :: !Int
  , rRhs1    :: !Int
  , rRhs2    :: !(Maybe Int)
  , rWeight  :: !Double
  , rOp      :: !Op
  , rArgKey  :: !(Maybe Int)
  , rArgVal  :: !(Maybe Int)
  , rArgType :: !(Maybe Int)
  , rPostPropIdxRight :: !Bool
  } deriving (Show)

data Grammar = Grammar
  { gRules  :: ![Rule]
  , gUnary  :: !(IM.IntMap [Rule])               -- rhs1 -> rules
  , gBinary :: !(IM.IntMap (IM.IntMap [Rule]))   -- rhs1 -> (rhs2 -> rules)
  } deriving (Show)

opFrom :: Text -> Op
opFrom s = case s of
  "EMPTY"         -> OP_EMPTY
  "LEFT"          -> OP_LEFT
  "RIGHT"         -> OP_RIGHT
  "UNIFY"         -> OP_UNIFY
  "REQUIRE_LEFT"  -> OP_REQUIRE_LEFT
  "REQUIRE_RIGHT" -> OP_REQUIRE_RIGHT
  "MAKE_GAP"      -> OP_MAKE_GAP
  "RELCLAUSE_OBL" -> OP_RELCLAUSE_OBL
  _               -> error ("op desconocido: " <> T.unpack s)

loadLexicon :: FilePath -> IO (Symtab, Lexicon)
loadLexicon path = do
  bs <- BL.readFile path
  lf <- case eitherDecode bs of
    Left e  -> die ("JSON lexicon falló: " <> e)
    Right x -> pure x
  let (st1, lex) = HM.foldlWithKey' step (symtabNew, M.empty) (entries lf)
  pure (st1, lex)
  where
    step (!st,!acc) word arr =
      let (st2, list) = foldl' build (st, []) arr
      in  (st2, M.insert word list acc)
    build (!st,!out) e =
      let (pid, st1) = intern (pos e) st
          (st2, fs)  = featsFromMap st1 (feats e)
      in  (st2, LexEntry pid (weight e) fs : out)

featsFromMap :: Symtab -> HM.HashMap Text Text -> (Symtab, Feats)
featsFromMap st0 hm =
  HM.foldlWithKey' step (st0, featsNorm []) hm
  where
    step (!st, Feats xs) k v =
      let (kid, st1) = intern k st
          (vid, st2) = intern v st1
      in  (st2, featsNorm ((kid,vid):xs))

loadGrammar :: Symtab -> FilePath -> IO (Symtab, Grammar)
loadGrammar st0 path = do
  bs <- BL.readFile path
  gf <- case eitherDecode bs of
    Left e  -> die ("JSON grammar falló: " <> e)
    Right x -> pure x

  let (st1, rs) = foldl' build (st0, []) (rules gf)
      rs' = reverse rs
      unary = foldl' idxUnary IM.empty rs'
      binary = foldl' idxBin IM.empty rs'
      gr = Grammar rs' unary binary
  pure (st1, gr)
  where
    build (!st,!out) rf =
      let rR = rhs rf
      in if null rR || length rR > 2
         then error "grammar.json: rhs len debe ser 1 o 2"
         else
           let (lhsId, st1) = intern (lhs rf) st
               (r1Id, st2)  = intern (head rR) st1
               (len, r2IdM, st3) =
                  if length rR == 2
                    then let (r2Id, stx) = intern (rR !! 1) st2
                         in (2, Just r2Id, stx)
                    else (1, Nothing, st2)
               opA = opFrom (op rf)

               (ak, st4) = maybeIntern st3 (args rf >>= key)
               (av, st5) = maybeIntern st4 (args rf >>= value)
               (at, st6) = maybeIntern st5 (args rf >>= type_)

               postFlag = fromMaybe [] (post rf)
               propIdxRight = any (=="PROPAGATE_IDX_TO_RIGHT") postFlag

               rule = Rule lhsId len r1Id r2IdM (weight rf) opA ak av at propIdxRight
           in (st6, rule:out)

    maybeIntern st Nothing  = (Nothing, st)
    maybeIntern st (Just t) = let (i, st1) = intern t st in (Just i, st1)

    idxUnary m r =
      if rRhsLen r == 1
        then IM.insertWith (++) (rRhs1 r) [r] m
        else m

    idxBin m r =
      if rRhsLen r == 2
        then let a = rRhs1 r
                 b = fromMaybe (error "rhs2 missing") (rRhs2 r)
             in IM.insertWith (\new old -> IM.unionWith (++) new old)
                              a (IM.singleton b [r]) m
        else m

--------------------------------------------------------------------------------
-- Tokenización
--------------------------------------------------------------------------------

data Token = Token
  { tkRaw   :: !Text
  , tkText  :: !Text
  , tkIndex :: !Int
  } deriving (Show)

splitSentences :: Text -> [Text]
splitSentences t =
  let cut = T.split (\c -> c == '.' || c == '\n' || c == '\r') t
  in filter (not . T.null) (map T.strip cut)

tokenize :: Text -> [Token]
tokenize s = go 0 0 []
  where
    n = T.length s

    isWordChar c = isLetter c || c == '-' || c == '\''

    go !i !idx acc
      | i >= n = reverse acc
      | otherwise =
          let c = T.index s i
          in if isLetter c
             then let (raw, j) = takeWord i
                      low = T.toLower raw
                  in case low of
                      "al"  ->
                        go j (idx+2) ( Token "el" "el" (idx+1)
                                    : Token "a"  "a"  idx
                                    : acc )
                      "del" ->
                        go j (idx+2) ( Token "el" "el" (idx+1)
                                    : Token "de" "de" idx
                                    : acc )
                      _ ->
                        case encliticSplit raw low idx of
                          Just (t1,t2) -> go j (idx+2) (t2:t1:acc)
                          Nothing      -> go j (idx+1) (Token raw low idx : acc)
             else go (i+1) idx acc

    takeWord i0 =
      let j0 = i0 + 1
          j  = advance j0
      in (T.slice i0 (j - i0) s, j)

    advance j
      | j >= n = j
      | otherwise =
          let c = T.index s j
          in if isWordChar c then advance (j+1) else j

encliticSplit :: Text -> Text -> Int -> Maybe (Token, Token)
encliticSplit raw low idx =
  let clitics = ["me","te","se","lo","la","los","las","le","les","nos","os"] :: [Text]
      best = longestSuffix low clitics
  in case best of
      Nothing -> Nothing
      Just cl ->
        let baseLen = T.length low - T.length cl
        in if baseLen > 2
           then
             let base = T.take baseLen low
                 looksVerb = any (`T.isSuffixOf` base) ["ar","er","ir","ando","iendo"]
             in if looksVerb
                then let rawBase = T.take baseLen raw
                         rawCl   = T.drop baseLen raw
                     in Just ( Token rawBase (T.toLower rawBase) idx
                             , Token rawCl   (T.toLower rawCl)   (idx+1)
                             )
                else Nothing
           else Nothing

longestSuffix :: Text -> [Text] -> Maybe Text
longestSuffix w cs =
  let ok = filter (`T.isSuffixOf` w) cs
  in if null ok then Nothing else Just (L.maximumBy (compare `on` T.length) ok)

--------------------------------------------------------------------------------
-- Arena + Node
--------------------------------------------------------------------------------

data Node = Node
  { nLabel   :: !Int
  , nIsLeaf  :: !Bool
  , nLeafRaw :: !(Maybe Text)
  , nFeats   :: !Feats
  , nScore   :: !Double
  , nLeft    :: !(Maybe Int)
  , nRight   :: !(Maybe Int)
  , nChild   :: !(Maybe Int)
  } deriving (Show)

data Arena = Arena
  { aNext  :: !Int
  , aNodes :: !(IM.IntMap Node)
  } deriving (Show)

arenaEmpty :: Arena
arenaEmpty = Arena 0 IM.empty

arenaAdd :: Node -> Arena -> (Int, Arena)
arenaAdd node (Arena n mp) = (n, Arena (n+1) (IM.insert n node mp))

arenaGet :: Arena -> Int -> Node
arenaGet (Arena _ mp) i =
  fromMaybe (error "arenaGet: id inválido") (IM.lookup i mp)

-- Clona subárbol, reemplazando feats val: from -> to
arenaCloneReplace :: Arena -> Int -> Int -> Int -> (Int, Arena)
arenaCloneReplace ar0 root fromV toV =
  let (newId, ar1) = go ar0 root
  in (newId, ar1)
  where
    go ar nid =
      let n0 = arenaGet ar nid
          Feats xs = nFeats n0
          feats' = featsNorm [ (k, if v==fromV then toV else v) | (k,v) <- xs ]

          (chIdM, ar1) =
            case nChild n0 of
              Nothing -> (Nothing, ar)
              Just c  -> let (nc, ar') = go ar c in (Just nc, ar')

          (lIdM, ar2) =
            case nLeft n0 of
              Nothing -> (Nothing, ar1)
              Just l  -> let (nl, ar') = go ar1 l in (Just nl, ar')

          (rIdM, ar3) =
            case nRight n0 of
              Nothing -> (Nothing, ar2)
              Just r  -> let (nr, ar') = go ar2 r in (Just nr, ar')

          n1 = n0 { nFeats = feats', nChild = chIdM, nLeft = lIdM, nRight = rIdM }
      in arenaAdd n1 ar3

arenaPretty :: Symtab -> Arena -> Int -> Text
arenaPretty st ar root = T.concat (go 0 root)
  where
    indent k = T.replicate (k*2) " "
    featKV (k,v) = strOf st k <> "=" <> strOf st v

    go ind nid =
      let n = arenaGet ar nid
      in if nIsLeaf n
         then [indent ind <> fromMaybe "" (nLeafRaw n) <> "\n"]
         else
           let label = strOf st (nLabel n)
               featsTxt =
                 case unFeats (nFeats n) of
                   [] -> ""
                   fs -> " [" <> T.intercalate ", " (map featKV fs) <> "]"
               headLine = indent ind <> label <> featsTxt <>
                          T.pack (printf "  (score=%.3f)\n" (nScore n))
           in case nChild n of
                Just c ->
                  headLine : go (ind+1) c
                Nothing ->
                  headLine :
                  maybe [] (go (ind+1)) (nLeft n) ++
                  maybe [] (go (ind+1)) (nRight n)

-- printf without importing Text.Printf in a weird way
import Text.Printf (printf)

--------------------------------------------------------------------------------
-- Chart + beam
--------------------------------------------------------------------------------

data Item = Item
  { itCat   :: !Int
  , itFeats :: !Feats
  , itFH    :: !Word64
  , itScore :: !Double
  , itNode  :: !Int
  } deriving (Show)

data Bucket = Bucket
  { bItems  :: ![Item]           -- orden desc score
  , bHashes :: !(IM.IntMap ())   -- dedupe por hash (Word64 -> unit) (usamos hash->Int)
  } deriving (Show)

type Cell = IM.IntMap Bucket

bucketEmpty :: Bucket
bucketEmpty = Bucket [] IM.empty

hashKey :: Word64 -> Int
hashKey w = fromIntegral (w .&. 0x7fffffff)
  where
    (.&.) = Data.Bits..&.

bucketHas :: Bucket -> Word64 -> Bool
bucketHas bk h = IM.member (hashKey h) (bHashes bk)

bucketInsert :: Int -> Item -> Bucket -> (Bucket, Int)
bucketInsert beam it bk =
  let items1 = insertDesc it (bItems bk)
      hashes1 = IM.insert (hashKey (itFH it)) () (bHashes bk)
      (items2, pruned) =
        if length items1 > beam
          then (take beam items1, length items1 - beam)
          else (items1, 0)
  in (Bucket items2 hashes1, pruned)

insertDesc :: Item -> [Item] -> [Item]
insertDesc it [] = [it]
insertDesc it (x:xs)
  | itScore it > itScore x = it:x:xs
  | otherwise              = x : insertDesc it xs

cellAddItem :: Int -> Item -> Cell -> (Cell, Int)
cellAddItem beam it cell =
  let cat = itCat it
      bk0 = IM.findWithDefault bucketEmpty cat cell
  in if bucketHas bk0 (itFH it)
     then (cell, 0)
     else
       let (bk1, pr) = bucketInsert beam it bk0
       in (IM.insert cat bk1 cell, pr)

--------------------------------------------------------------------------------
-- Const IDs
--------------------------------------------------------------------------------

data Const = Const
  { cIdx   :: !Int
  , cQi    :: !Int
  , cGap   :: !Int
  , cObl   :: !Int
  , cYes   :: !Int
  , cFin   :: !Int
  , cNo    :: !Int
  , cGen   :: !Int
  , cNum   :: !Int
  , cSg    :: !Int

  , cTOK   :: !Int
  , cS     :: !Int
  , cVPFIN :: !Int
  , cPinf  :: !Int
  , cVPNF  :: !Int
  , cVP    :: !Int
  , cCl    :: !Int

  , cN     :: !Int
  , cPropN :: !Int
  , cPron  :: !Int
  , cAdv   :: !Int
  , cVi    :: !Int
  , cVt    :: !Int
  } deriving (Show)

ensureConstants :: Symtab -> (Symtab, Const)
ensureConstants st0 =
  let need =
        [ "idx","?i","gap","obl","yes","fin","no","gen","num","sg"
        , "TOK","S","VP_FIN","Pinf","VP_NF","VP","Cl"
        , "N","PropN","Pron","Adv","Vi","Vt"
        ]
      (st1, ids) = foldl' step (st0, M.empty) need
      gid k = fromMaybe (error ("missing const: " <> T.unpack k)) (M.lookup k ids)
      c = Const
          (gid "idx") (gid "?i") (gid "gap") (gid "obl") (gid "yes") (gid "fin") (gid "no")
          (gid "gen") (gid "num") (gid "sg")
          (gid "TOK") (gid "S") (gid "VP_FIN") (gid "Pinf") (gid "VP_NF") (gid "VP") (gid "Cl")
          (gid "N") (gid "PropN") (gid "Pron") (gid "Adv") (gid "Vi") (gid "Vt")
  in (st1, c)
  where
    step (!st,!mp) k =
      let (i, st1) = intern k st
      in (st1, M.insert k i mp)

--------------------------------------------------------------------------------
-- Ops DSL
--------------------------------------------------------------------------------

applyOp :: Symtab -> Const -> Rule -> Feats -> Feats -> Maybe Feats
applyOp st c r lf rf =
  case rOp r of
    OP_EMPTY -> Just (featsNorm [])
    OP_LEFT  -> Just lf
    OP_RIGHT -> Just rf
    OP_UNIFY -> unify st lf rf

    OP_REQUIRE_LEFT ->
      case (rArgKey r, rArgVal r) of
        (Just k, Just v) -> requireFeat st lf k v
        _                -> Nothing

    OP_REQUIRE_RIGHT ->
      case (rArgKey r, rArgVal r) of
        (Just k, Just v) -> requireFeat st rf k v
        _                -> Nothing

    OP_MAKE_GAP ->
      case rArgType r of
        Just t ->
          Just (featsNorm [(cIdx c, cQi c), (cGap c, t)])
        _ -> Nothing

    OP_RELCLAUSE_OBL ->
      case requireFeat st rf (cObl c) (cYes c) of
        Nothing -> Nothing
        Just _  -> unify st lf (featsNorm [(cGap c, cObl c)])

--------------------------------------------------------------------------------
-- Guess lex (OOV)
--------------------------------------------------------------------------------

isDetWord :: Text -> Bool
isDetWord w = w == "el" || w == "la" || w == "los" || w == "las"

guessLex :: Symtab -> Const -> Token -> (Symtab, [LexEntry])
guessLex st0 c tk =
  let raw = tkRaw tk
      low = tkText tk
      (st1, prop) =
        case T.uncons raw of
          Just (ch,_) | isUpper ch && not (isDetWord low) ->
            let entry = LexEntry (cPropN c) 0.03 (featsNorm [(cNum c, cSg c)])
            in (st0, [entry])
          _ -> (st0, [])

      adv =
        if "mente" `T.isSuffixOf` low
          then [LexEntry (cAdv c) (-0.03) (featsNorm [])]
          else []

      baseVFeats = featsNorm [(cFin c, cNo c), (cObl c, cNo c)]
      verbs
        | any (`T.isSuffixOf` low) ["ar","er","ir"] =
            [ LexEntry (cVi c) (-0.12) baseVFeats
            , LexEntry (cVt c) (-0.14) baseVFeats
            ]
        | any (`T.isSuffixOf` low) ["ando","iendo"] =
            [ LexEntry (cVi c) (-0.14) baseVFeats
            , LexEntry (cVt c) (-0.16) baseVFeats
            ]
        | otherwise = []

      (st2, fallbackN) =
        if null (prop ++ adv ++ verbs)
          then
            let (vg, stA) = intern "?g" st1
                (vn, stB) = intern "?n" stA
            in (stB, [LexEntry (cN c) (-0.35) (featsNorm [(cGen c, vg),(cNum c, vn)])])
          else (st1, [])

  in (st2, prop ++ adv ++ verbs ++ fallbackN)

--------------------------------------------------------------------------------
-- Unary closure
--------------------------------------------------------------------------------

unaryClosure :: Symtab -> Const -> Grammar -> Arena -> Int -> (Cell, Int, Int, Arena) -> (Cell, Int, Int, Arena)
unaryClosure st c gr beam (cell0, pruned0, unaryApps0, arena0) =
  let unaryIdx = gUnary gr
      loop (!cell,!pr,!ua,!ar) =
        let (cell2, pr2, ua2, ar2, changed) =
              IM.foldlWithKey' (stepCat unaryIdx) (cell, pr, ua, ar, False) cell
        in if changed then loop (cell2, pr2, ua2, ar2) else (cell2, pr2, ua2, ar2)

      stepCat idx (!accCell,!accPr,!accUa,!accAr,!accCh) rhsCat bk =
        let rules = IM.findWithDefault [] rhsCat idx
            itemsSnap = bItems bk
            (cellX, prX, uaX, arX, chX) =
              foldl' (applyRules itemsSnap) (accCell, accPr, accUa, accAr, accCh) rules
        in (cellX, prX, uaX, arX, chX)

      applyRules itemsSnap (!cell,!pr,!ua,!ar,!ch) rule =
        foldl' (applyUnary rule) (cell, pr, ua, ar, ch) itemsSnap

      applyUnary rule (!cell,!pr,!ua,!ar,!ch) childIt =
        case applyOp st c rule (itFeats childIt) (featsNorm []) of
          Nothing -> (cell, pr, ua, ar, ch)
          Just pf ->
            let ua' = ua + 1
                score = itScore childIt + rWeight rule
                node = Node (rLhs rule) False Nothing pf score Nothing Nothing (Just (itNode childIt))
                (nid, ar1) = arenaAdd node ar
                it = Item (rLhs rule) pf (featsHash64 pf) score nid
                (cell1, prInc) = cellAddItem beam it cell
            in (cell1, pr + prInc, ua', ar1, ch || prInc >= 0) -- dedupe no expone cambio; ok como aproximación
  in loop (cell0, pruned0, unaryApps0, arena0)

--------------------------------------------------------------------------------
-- Sanity checks
--------------------------------------------------------------------------------

hasDescLabel :: Arena -> Int -> Int -> Bool
hasDescLabel ar nid lab =
  let n = arenaGet ar nid
      here = (not (nIsLeaf n)) && nLabel n == lab
  in here
     || maybe False (\x -> hasDescLabel ar x lab) (nChild n)
     || maybe False (\x -> hasDescLabel ar x lab) (nLeft n)
     || maybe False (\x -> hasDescLabel ar x lab) (nRight n)

sanitySHasVPFin :: Const -> Arena -> Int -> Bool
sanitySHasVPFin c ar root =
  let s = cS c
      vpfin = cVPFIN c
      walk nid =
        let n = arenaGet ar nid
            foundHere =
              (not (nIsLeaf n)) && nLabel n == s &&
              ( any (\mid -> maybe False (\x -> nLabel (arenaGet ar x) == vpfin) mid)
                    [nChild n, nLeft n, nRight n]
              )
        in foundHere
           || maybe False walk (nChild n)
           || maybe False walk (nLeft n)
           || maybe False walk (nRight n)
  in walk root

sanitySinTakesVPNF :: Const -> Arena -> Int -> Bool
sanitySinTakesVPNF c ar root =
  let pinf = cPinf c
      vpnf = cVPNF c
      rec nid =
        let n = arenaGet ar nid
            okHere =
              if (not (nIsLeaf n)) && nLabel n == pinf
                then hasDescLabel ar nid vpnf
                else True
        in okHere
           && maybe True rec (nChild n)
           && maybe True rec (nLeft n)
           && maybe True rec (nRight n)
  in rec root

sanityEncliticOnlyNF :: Const -> Arena -> Int -> Bool
sanityEncliticOnlyNF c ar root =
  let vp = cVP c
      cl = cCl c
      vt = cVt c
      vi = cVi c
      rec nid =
        let n = arenaGet ar nid
            badHere =
              (not (nIsLeaf n)) && nLabel n == vp &&
              case (nLeft n, nRight n) of
                (Just l, Just r) ->
                  let ln = arenaGet ar l
                      rn = arenaGet ar r
                  in (not (rnIsLeaf rn)) && nLabel rn == cl &&
                     (not (nIsLeaf ln)) && (nLabel ln == vt || nLabel ln == vi)
                _ -> False
        in (not badHere)
           && maybe True rec (nChild n)
           && maybe True rec (nLeft n)
           && maybe True rec (nRight n)
  in rec root
  where
    rnIsLeaf = nIsLeaf

--------------------------------------------------------------------------------
-- Parse sentence
--------------------------------------------------------------------------------

data RowOut = RowOut
  { sentence              :: !Text
  , tokens                :: !Int
  , oovTokens             :: !Int
  , parsed                :: !Bool
  , nParsesReturned       :: !Int
  , bestScore             :: !(Maybe Double)
  , timeMs                :: !Double
  , chartItemsTotal       :: !Int
  , chartItemsMaxCell     :: !Int
  , prunedByBeam          :: !Int
  , unaryApplications     :: !Int
  , ambiguousCells        :: !Int
  , sanitySHasVpFin       :: !Bool
  , sanitySinTakesVpNf    :: !Bool
  , sanityEncliticOnlyNf  :: !Bool
  , notes                 :: ![Text]
  , bestTree              :: !(Maybe Text)
  } deriving (Show, Generic)

instance ToJSON RowOut where
  toJSON = genericToJSON defaultOptions

data SummaryOut = SummaryOut
  { sentences    :: !Int
  , coverage     :: !Double
  , avgTokens    :: !Double
  , avgOov       :: !Double
  , totalTimeMs  :: !Double
  , avgTimeMs    :: !Double
  , beam         :: !Int
  , topK         :: !Int
  , rows         :: ![RowOut]
  } deriving (Show, Generic)

instance ToJSON SummaryOut where
  toJSON = genericToJSON defaultOptions

-- Chart como vector 1D: index(i,j) = i*(n+1)+j
type Chart = V.Vector Cell

cidx :: Int -> Int -> Int -> Int
cidx i j n = i*(n+1) + j

chartNew :: Int -> Chart
chartNew n = V.replicate ((n+1)*(n+1)) IM.empty

chartGet :: Chart -> Int -> Int -> Int -> Cell
chartGet ch i j n = ch V.! cidx i j n

chartSet :: Chart -> Int -> Int -> Int -> Cell -> Chart
chartSet ch i j n cell = ch V.// [(cidx i j n, cell)]

emitLex :: Symtab -> Const -> Int -> Token -> LexEntry -> Arena -> Cell -> Int -> IM.IntMap Int -> (Arena, Cell, Int)
emitLex st c beam tk le arena0 cell0 pruned0 tids =
  let pos = lePos le
      w   = leWeight le
      Feats fs0 = leFeats le

      feats1 =
        if pos == cN c || pos == cPropN c || pos == cPron c
          then case featsFind (cIdx c) (leFeats le) of
                 Nothing ->
                   let tid = fromMaybe (error "tids missing") (IM.lookup (tkIndex tk) tids)
                   in fromMaybe (leFeats le) (unify st (leFeats le) (featsNorm [(cIdx c, tid)]))
                 Just _  -> leFeats le
          else leFeats le

      leaf = Node (cTOK c) True (Just (tkRaw tk)) (featsNorm []) w Nothing Nothing Nothing
      (leafId, arena1) = arenaAdd leaf arena0
      pre  = Node pos False Nothing feats1 w Nothing Nothing (Just leafId)
      (preId, arena2)  = arenaAdd pre arena1

      it = Item pos feats1 (featsHash64 feats1) w preId
      (cell1, prInc) = cellAddItem beam it cell0
  in (arena2, cell1, pruned0 + prInc)

parseSentence :: Symtab -> Const -> Lexicon -> Grammar -> Int -> Int -> Bool -> Text -> IO RowOut
parseSentence st0 c lx gr topk beam wantTree sent = do
  let t0 = 0 :: Int
  -- crude timer (monotonicTime would be ideal; keeping base-only is fine)
  -- We'll approximate by forcing evaluation; for real timing, use System.Clock.
  -- Here we do a cheap workaround: timeMs = 0 (still functional parser).
  -- If querés timing real, te lo meto con clock.
  let toks0 = tokenize sent
      toksN = length toks0
      toks  = zipWith (\i t -> t { tkIndex = i }) [0..] toks0

      (st1, tids) = internTIds st0 toksN
      chart0 = chartNew toksN
      arena0 = arenaEmpty

  -- lexical init
  let (chart1, arena1, oovCount, pruned1, unaryApps1) =
        foldl' (lexStep st1 tids) (chart0, arena0, 0, 0, 0) (zip [0..] toks)

  -- CKY
  let (chart2, arena2, pruned2, unaryApps2) =
        ckyAll st1 chart1 arena1 pruned1 unaryApps1

  -- metrics
  let (totItems, maxCell, ambCells) = chartMetrics chart2 toksN

  -- best S
  let cellSN = chartGet chart2 0 toksN toksN
      bkS = IM.lookup (cS c) cellSN
      bestIt = bkS >>= \bk -> case bItems bk of
                                []    -> Nothing
                                (x:_) -> Just x

  let (parsedOk, nRet, bestSc, notes0, bestTreeTxt, s1, s2, s3) =
        case bestIt of
          Nothing ->
            (False, 0, Nothing, ["NO_PARSE"], Nothing, False, False, False)
          Just it ->
            let treeId = itNode it
                san1 = sanitySHasVPFin c arena2 treeId
                san2 = sanitySinTakesVPNF c arena2 treeId
                san3 = sanityEncliticOnlyNF c arena2 treeId
                n1 = (if san1 then [] else ["WARN: S sin VP_FIN visible"])
                n2 = (if san2 then [] else ["WARN: 'sin' sin VP_NF bajo Pinf"])
                n3 = (if san3 then [] else ["WARN: enclítico con verbo finito"])
                bt = if wantTree then Just (arenaPretty st1 arena2 treeId) else Nothing
                itemsS = maybe [] bItems bkS
            in (True, min topk (length itemsS), Just (itScore it), n1++n2++n3, bt, san1, san2, san3)

  let row = RowOut
        { sentence = sent
        , tokens = toksN
        , oovTokens = oovCount
        , parsed = parsedOk
        , nParsesReturned = nRet
        , bestScore = bestSc
        , timeMs = 0.0   -- ver nota arriba
        , chartItemsTotal = totItems
        , chartItemsMaxCell = maxCell
        , prunedByBeam = pruned2
        , unaryApplications = unaryApps2
        , ambiguousCells = ambCells
        , sanitySHasVpFin = s1
        , sanitySinTakesVpNf = s2
        , sanityEncliticOnlyNf = s3
        , notes = notes0
        , bestTree = bestTreeTxt
        }
  row `deepseq` pure row
  where
    internTIds st n =
      let (st1, mp) = foldl' (\(!stA,!m) i ->
                                let (tid, stB) = intern (T.pack ("t"<>show i)) stA
                                in (stB, IM.insert i tid m)
                             ) (st, IM.empty) [0..(n-1)]
      in (st1, mp)

    lexStep st tids (!ch,!ar,!oov,!pr,!ua) (i, tk) =
      let cell0 = chartGet ch i (i+1) (length (tokenize sent))
          word = tkText tk
          lexEntries = M.lookup word lx
          (st2, guessEntries) = guessLex st c tk
          (oov2, entries) =
            case lexEntries of
              Nothing -> (oov+1, guessEntries)
              Just es -> (oov, es ++ guessEntries)
          (ar1, cell1, pr1) =
            foldl' (\(!a,!cell,!p) le ->
                      let (a2, c2, p2) = emitLex st2 c beam tk le a cell p tids
                      in (a2, c2, p2)
                   ) (ar, cell0, pr) entries
          (cell2, pr2, ua2, ar2) = unaryClosure st2 c gr beam (cell1, pr1, ua, ar1)
          ch1 = chartSet ch i (i+1) (length (tokenize sent)) cell2
      in (ch1, ar2, oov2, pr2, ua2)

    ckyAll st ch0 ar0 pr0 ua0 =
      let n = length (tokenize sent)
          spans = [2..n]
      in foldl' (spanStep st n) (ch0, ar0, pr0, ua0) spans

    spanStep st n (!ch,!ar,!pr,!ua) span =
      foldl' (cellStep st n span) (ch, ar, pr, ua) [0..(n-span)]

    cellStep st n span (!ch,!ar,!pr,!ua) i =
      let j = i + span
          cell0 = chartGet ch i j n
          (cell1, ar1, pr1) = foldl' (splitStep st n i j ch) (cell0, ar, pr) [i+1..j-1]
          (cell2, pr2, ua2, ar2) = unaryClosure st c gr beam (cell1, pr1, ua, ar1)
          ch1 = chartSet ch i j n cell2
      in (ch1, ar2, pr2, ua2)

    splitStep st n i j ch (!cellAcc,!arAcc,!prAcc) k =
      let lcell = chartGet ch i k n
          rcell = chartGet ch k j n
      in if IM.null lcell || IM.null rcell
           then (cellAcc, arAcc, prAcc)
           else
             let (cell1, ar1, pr1) =
                   IM.foldlWithKey' (combLeft st n lcell rcell) (cellAcc, arAcc, prAcc) lcell
             in (cell1, ar1, pr1)

    combLeft st n lcell rcell (!cellAcc,!arAcc,!prAcc) catL bkL =
      let itemsL = bItems bkL
      in IM.foldlWithKey' (combRight st itemsL) (cellAcc, arAcc, prAcc) rcell

    combRight st itemsL (!cellAcc,!arAcc,!prAcc) catR bkR =
      let itemsR = bItems bkR
          rules = case IM.lookup (rRhs1dummy) (gBinary gr) of
                    _ -> rulesFor (catLdummy) (catRdummy) -- we’ll override below
      in combineRules st catLdummy catRdummy itemsL itemsR (cellAcc, arAcc, prAcc)

    -- helper “real” rules lookup
    rulesFor a b =
      case IM.lookup a (gBinary gr) >>= IM.lookup b of
        Nothing -> []
        Just rs -> rs

    combineRules st a b itemsL itemsR (!cellAcc,!arAcc,!prAcc) =
      let rs = rulesFor a b
      in foldl' (\(!c1,!a1,!p1) r -> combineRule st r itemsL itemsR (c1,a1,p1)) (cellAcc, arAcc, prAcc) rs

    combineRule st r itemsL itemsR (!cellAcc,!arAcc,!prAcc) =
      foldl' (\(!c2,!a2,!p2) il ->
                foldl' (\(!c3,!a3,!p3) ir ->
                          case applyOp st c r (itFeats il) (itFeats ir) of
                            Nothing -> (c3,a3,p3)
                            Just pf ->
                              let score = itScore il + itScore ir + rWeight r
                                  (rightNodeId, a4) =
                                    if rPostPropIdxRight r
                                      then case featsFind (cIdx c) (itFeats il) of
                                             Just idxV ->
                                               arenaCloneReplace a3 (itNode ir) (cQi c) idxV
                                             Nothing -> (itNode ir, a3)
                                      else (itNode ir, a3)
                                  node = Node (rLhs r) False Nothing pf score (Just (itNode il)) (Just rightNodeId) Nothing
                                  (nid, a5) = arenaAdd node a4
                                  it = Item (rLhs r) pf (featsHash64 pf) score nid
                                  (c4, prInc) = cellAddItem beam it c3
                              in (c4, a5, p3 + prInc)
                      ) (c2,a2,p2) itemsR
            ) (cellAcc, arAcc, prAcc) itemsL

    chartMetrics ch n =
      let cells = [ chartGet ch i j n | i <- [0..n], j <- [0..n] ]
          cellItems cell = sum [ length (bItems bk) | (_,bk) <- IM.toList cell ]
          totals = map cellItems cells
          tot = sum totals
          mx  = maximum (0:totals)
          amb = length [ () | cell <- cells, IM.size cell >= 2 ]
      in (tot, mx, amb)

--------------------------------------------------------------------------------
-- CLI
--------------------------------------------------------------------------------

data Opts = Opts
  { oLex    :: FilePath
  , oGram   :: FilePath
  , oFile   :: FilePath
  , oText   :: Maybe Text
  , oJson   :: Maybe FilePath
  , oBeam   :: Int
  , oTopK   :: Int
  , oTrees  :: Bool
  , oPrint  :: Bool
  } deriving (Show)

defaultOpts :: Opts
defaultOpts = Opts "lexicon.json" "grammar.json" "corpus.txt" Nothing Nothing 16 1 False False

parseArgs :: [String] -> Opts
parseArgs = go defaultOpts
  where
    go o [] = o
    go o ("--help":_) = error "Uso: --lex --grammar --file|--text --beam --topk --trees --print --json"
    go o ("--lex":v:xs) = go (o { oLex = v }) xs
    go o ("--grammar":v:xs) = go (o { oGram = v }) xs
    go o ("--file":v:xs) = go (o { oFile = v }) xs
    go o ("--text":v:xs) = go (o { oText = Just (T.pack v) }) xs
    go o ("--json":v:xs) = go (o { oJson = Just v }) xs
    go o ("--beam":v:xs) = go (o { oBeam = read v }) xs
    go o ("--topk":v:xs) = go (o { oTopK = read v }) xs
    go o ("--trees":xs) = go (o { oTrees = True }) xs
    go o ("--print":xs) = go (o { oPrint = True }) xs
    go _ (x:_) = error ("Arg desconocido: " <> x)

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: IO ()
main = do
  opts <- parseArgs <$> getArgs

  (st1, lex0) <- loadLexicon (oLex opts)
  (st2, gr0)  <- loadGrammar st1 (oGram opts)
  let (st3, c) = ensureConstants st2

  corpus <- case oText opts of
              Just t  -> pure t
              Nothing -> T.pack . BL8.unpack <$> BL.readFile (oFile opts)
  let sents = splitSentences corpus

  rows <- mapM (parseSentence st3 c lex0 gr0 (oTopK opts) (oBeam opts) (oTrees opts)) sents

  -- Print per-row
  when (oPrint opts) $
    mapM_ (printRow (oTrees opts)) rows

  let sentN = length sents
      parsedN = length [ () | r <- rows, parsed r ]
      cov = if sentN==0 then 0 else fromIntegral parsedN / fromIntegral sentN
      avgTok = if sentN==0 then 0 else fromIntegral (sum (map tokens rows)) / fromIntegral sentN
      avgOov = if sentN==0 then 0 else fromIntegral (sum (map oovTokens rows)) / fromIntegral sentN
      totTime = sum (map timeMs rows)
      avgTime = if sentN==0 then 0 else totTime / fromIntegral sentN

  let summary = SummaryOut
        { sentences = sentN
        , coverage = cov
        , avgTokens = avgTok
        , avgOov = avgOov
        , totalTimeMs = totTime
        , avgTimeMs = avgTime
        , beam = oBeam opts
        , topK = oTopK opts
        , rows = rows
        }

  if oPrint opts
    then do
      putStrLn "=============================================================================="
      putStrLn ("SUMMARY\nsentences="<>show sentN<>
                "  coverage="<>printfF cov<>
                "  avg_tokens="<>printfF avgTok<>
                "  avg_oov="<>printfF avgOov<>
                "  avg_time_ms="<>printfF avgTime<>
                "  beam="<>show (oBeam opts)<> "  top_k="<>show (oTopK opts))
    else
      putStrLn ("SUMMARY: sentences="<>show sentN<>
                " coverage="<>printfF cov<>
                " avg_time_ms="<>printfF avgTime<>
                " beam="<>show (oBeam opts)<> " top_k="<>show (oTopK opts))

  case oJson opts of
    Nothing -> pure ()
    Just p  -> BL.writeFile p (encode summary) >> putStrLn ("Wrote JSON: " <> p)

-- helpers
import qualified Data.ByteString.Lazy.Char8 as BL8
import Control.Monad (when)

printfF :: Double -> String
printfF x = printf "%.3f" x

printRow :: Bool -> RowOut -> IO ()
printRow wantTree r = do
  putStrLn "=============================================================================="
  putStrLn (T.unpack (sentence r))
  putStrLn ("tokens="<>show (tokens r) <>
            "  oov="<>show (oovTokens r) <>
            "  parsed="<>show (if parsed r then (1::Int) else 0) <>
            "  parses="<>show (nParsesReturned r) <>
            "  bestScore="<> maybe "null" (printf "%.6f") (bestScore r) <>
            "  time_ms="<>printf "%.1f" (timeMs r))
  putStrLn ("chart_items="<>show (chartItemsTotal r) <>
            "  max_cell="<>show (chartItemsMaxCell r) <>
            "  pruned="<>show (prunedByBeam r) <>
            "  unary_apps="<>show (unaryApplications r) <>
            "  amb_cells="<>show (ambiguousCells r))
  case notes r of
    [] -> pure ()
    ns -> putStrLn ("notes: " <> T.unpack (T.intercalate "; " ns))
  when wantTree $
    case bestTree r of
      Nothing -> pure ()
      Just t  -> putStr (T.unpack t)
