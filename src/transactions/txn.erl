-module(txn).

-export([
    create_oui_txn/4, create_oui_txn/5
]).

create_oui_txn(Owner, OwnerSigFun, Payer, Addresses) ->
    Txn = oui_txn:new(Owner, Addresses, Payer, 1, 1),
    SignedTxn = oui_txn:sign(Txn, OwnerSigFun),
    oui_txn:encode(SignedTxn).

create_oui_txn(Owner, OwnerSigFun, Payer, PayerSigFun, Addresses) ->
    Txn = oui_txn:new(Owner, Addresses, Payer, 1, 1),
    SignedTxn0 = oui_txn:sign(Txn, OwnerSigFun),
    SignedTxn1 = oui_txn:sign_payer(SignedTxn0, PayerSigFun),
    oui_txn:encode(SignedTxn1).
