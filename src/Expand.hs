module Expand where

import Syntax
import Match
import PatternQQ

data ExpansionRule =
  ExpansionRule {
  expansion_src :: CmdPattern
  , expansion_tgt :: CmdPattern
  } deriving (Show, Eq)

fuzziExpansionRules :: [ExpansionRule]
fuzziExpansionRules = [ bmapExpansionRule
                      , amapExpansionRule
                      , partitionExpansionRule
                      , bsumExpansionRule
                      , clearExpansionArrayRule
                      ]

expandPrefix :: Cmd -> ExpansionRule -> Cmd
expandPrefix c (ExpansionRule src tgt) =
  case runUniM $ matchCmdPrefix src c of
    Left _ -> c
    Right (env, Nothing) ->
      case subst @Cmd tgt env of
        Just cp' ->
          case recover @Cmd cp' of
            Just c' -> c'
            Nothing -> c
        Nothing -> c
    Right (env, Just remains) ->
      case subst @Cmd tgt env of
        Just cp' ->
          case recover @Cmd cp' of
            Just c' -> CSeq trivialPosition c' remains
            Nothing -> c
        Nothing -> c

-- |Tries to expand based on the expansion rule 1 step.
expand :: Cmd -> ExpansionRule -> Cmd
expand c (ExpansionRule src tgt) =
  case runUniM $ matchCmdAnywhere src c of
    Left _ -> c
    Right (env, hole) ->
      case subst @Cmd tgt env of
        Just cp' ->
          case recover @Cmd cp' of
            Just c' -> hole c'
            Nothing -> c
        Nothing -> c

-- |Applies each of the expansion rules for 1 step.
expandAll :: Cmd -> [ExpansionRule] -> Cmd
expandAll c rules = foldr (flip expand) c rules

-- |Expand using the depth limit.
desugar :: Cmd -> [ExpansionRule] -> Int -> Cmd
desugar c rules depth
  | depth <= 0 = c
  | otherwise  =
    let c' = expandAll c rules
    in if c == c'
       then c
       else desugar c' rules (depth - 1)

desugarFix :: Cmd -> [ExpansionRule] -> Cmd
desugarFix c rules =
  let c' = expandAll c rules
  in if c == c'
     then normalizeSeq c
     else desugarFix c' rules

bmapSrc :: CmdPattern
bmapSrc = [cpat|
bmap(v(in), v(out), v(t_in), v(idx), v(t_out), c(body))
|]

bmapTgt :: CmdPattern
bmapTgt = [cpat|
v(idx) = 0;
v(out) = {};
length(v(out)) = length(v(in));
while v(idx) < length(v(in)) do
  v(t_in) = (v(in))[v(idx)];
  { c(body) };
  v(out)[v(idx)] = v(t_out);
  v(idx) = v(idx) + 1;
end
|]

bmapExpansionRule :: ExpansionRule
bmapExpansionRule = ExpansionRule bmapSrc bmapTgt

amapSrc :: CmdPattern
amapSrc = [cpat|
amap(v(in), v(out), v(t_in), v(idx), v(t_out), c(body))
|]

amapTgt :: CmdPattern
amapTgt = [cpat|
v(idx) = 0;
v(out) = [];
length(v(out)) = length(v(in));
while v(idx) < length(v(in)) do
  v(t_in) = (v(in))[v(idx)];
  { c(body) };
  v(out)[v(idx)] = v(t_out);
  v(idx) = v(idx) + 1;
end
|]

amapExpansionRule :: ExpansionRule
amapExpansionRule = ExpansionRule amapSrc amapTgt

partitionSrc :: CmdPattern
partitionSrc = [cpat|
partition(v(in), v(out),
          v(t_in), v(idx), v(t_out),
          v(t_idx), v(out_idx),
          v(t_part), e(n_parts), c(body))
|]

partitionTgt :: CmdPattern
partitionTgt = [cpat|
v(idx) = 0;
length(v(out)) = e(n_parts);
while v(idx) < e(n_parts) do
  v(t_part) = v(out)[v(idx)];
  length(v(t_part)) = 0;
  v(out)[v(idx)] = v(t_part);
  v(idx) = v(idx) + 1;
end;

v(idx) = 0;
v(out_idx) = {};
length(v(out_idx)) = length(v(in));
while v(idx) < length(v(in)) do
  v(t_in) = (v(in))[v(idx)];
  { c(body) };
  v(out_idx)[v(idx)] = v(t_out);
  v(idx) = v(idx) + 1;
end;

while v(idx) < length(v(out_idx)) do
  v(t_idx) = (v(out_idx))[v(idx)];
  if 0 <= v(t_idx) && v(t_idx) < length(v(out_idx)) then
    v(t_part) = v(out)[v(t_idx)];
    length(v(t_part)) = length(v(t_part)) + 1;
    (v(t_part))[length(v(t_part)) - 1] = (v(in))[v(idx)];
    (v(out))[v(t_idx)] = v(t_part);
  else
    skip;
  end;
  v(idx) = v(idx) + 1;
end
|]

partitionExpansionRule :: ExpansionRule
partitionExpansionRule = ExpansionRule partitionSrc partitionTgt

bsumSrc :: CmdPattern
bsumSrc = [cpat|
bsum(v(in), v(out), v(idx), v(t_in), fesc(bound))
|]

bsumTgt :: CmdPattern
bsumTgt = [cpat|
v(idx) = 0;
v(out) = 0.0;
while v(idx) < length(v(in)) do
  v(t_in) = v(in)[v(idx)];
  if v(t_in) < fesc(bound) then
    v(out) = v(out) - fesc(bound);
  else
    if v(t_in) > fesc(bound) then
      v(out) = v(out) + fesc(bound);
    else
      v(out) = v(out) + v(t_in);
    end
  end;
  v(idx) = v(idx) + 1;
end
|]

bsumExpansionRule :: ExpansionRule
bsumExpansionRule = ExpansionRule bsumSrc bsumTgt

clearArraySrc :: CmdPattern
clearArraySrc = [cpat|
clear(v(arr), v(idx))
|]

clearArrayTgt :: CmdPattern
clearArrayTgt = [cpat|
v(idx) = 0;
while v(idx) < length(v(arr)) do
  v(arr)[v(idx)] = 0.0;
  v(idx) = v(idx) + 1
end;
|]

clearExpansionArrayRule :: ExpansionRule
clearExpansionArrayRule = ExpansionRule clearArraySrc clearArrayTgt
