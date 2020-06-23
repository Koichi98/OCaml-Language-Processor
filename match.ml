open Syntax

exception EvalErr

let rec find_match pattern value = 
  match pattern with
  |PInt i1 -> 
    (match value with 
    |VInt i2 -> if i1=i2 then Some [] else None
    |_ -> None)
  |PBool b1 -> 
    (match value with 
    |VBool b2 -> if b1=b2 then Some [] else None
    |_ -> None)
  |PVar name ->
    (match value with
    |VErr error -> None
    |rest -> Some [(name,rest)])
  |PPair (p1,p2) ->
    (match value with 
    |VPair (v1,v2) -> 
    let Some env1 = find_match p1 v1 in
    let Some env2 = find_match p2 v2 in
    Some (env1@env2)
    |_ -> None)
  |PNil ->
    (match value with 
    |VNil -> Some []
    |_ -> None)
  |PCons (p1,p2) -> 
    (match value with
    |VCons (v1,v2) -> 
    let Some env1 = find_match p1 v1 in
    let Some env2 = find_match p2 v2 in
    Some (env1@env2)
    |_ -> None)
