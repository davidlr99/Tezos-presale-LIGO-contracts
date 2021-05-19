#include "./libs/fa2_interface.mligo"

(* action *)
type order = {
    ref: address;
}
type action = 
| Buy of order
| Withdraw of tez

(* storage *)
type storage = {
  initaltacoz : tez;
  lastorder: address;
  owner: address;
  fa2contractaddress: address;
  fa2owneraddress:  address; 
}

(* other types *)

let buyTacoz (o,s: order * storage): operation list * storage = 
    if s.initaltacoz-Tezos.amount < 1tez then (failwith "All Tacoz sold." : operation list * storage)
    else
    let tacoz_amount : nat  = (Tezos.amount/1tez)*1_000_000_000_000_000_000n*120000n (* 1XTZ = 120000TCZ *)in 
    let receiver : (transfer list) contract =
      match (Tezos.get_entrypoint_opt "%transfer" s.fa2contractaddress: ((transfer list) contract) option) with
        Some (contract) -> contract
      | None -> (failwith ("Contract not found :/ .") : ((transfer list) contract)) in
    let tx : transfer = {
      from_ = s.fa2owneraddress;
      txs= [{
        to_ = Tezos.sender;
        token_id = 0n;
        amount = tacoz_amount;
    }]} in
    let tacoz_amount_ref : nat = (tacoz_amount*5n)/100n in
    let tx_ref : transfer = {
      from_ = s.fa2owneraddress;
      txs= [{
        to_ = o.ref;
        token_id = 0n;
        amount = tacoz_amount_ref;
    }]} in
    if o.ref <> Tezos.sender then 
        let operations : operation list = [Tezos.transaction [tx] 0mutez receiver; Tezos.transaction [tx_ref] 0mutez receiver;] in
        (operations), { s with
        initaltacoz = s.initaltacoz-Tezos.amount;
        lastorder =  o.ref;}
    else
        let operations : operation list = [Tezos.transaction [tx] 0mutez receiver;] in
        (operations), { s with
        initaltacoz = s.initaltacoz-Tezos.amount;
        lastorder =  o.ref;}


let withdrawTez (a, s: tez * storage): operation list * storage = 
    if s.fa2owneraddress <> Tezos.sender then (failwith "You are not the contract owner." : operation list * storage)
    else
    let receiver : unit contract =
      match (Tezos.get_contract_opt(s.owner) : unit contract option) with
        Some (contract) -> contract
      | None -> (failwith ("Contract not found.") : unit contract) in
    let tx : operation = Tezos.transaction unit a receiver in
    ([tx]: operation list), s



(* code *)
let main (p, s: action * storage): operation list * storage =
    match p with 
    | Buy order -> buyTacoz (order, s)
    | Withdraw amoun -> withdrawTez (amoun, s)
   