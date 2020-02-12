-module(security_exchange_txn).

-include_lib("helium_proto/include/blockchain_txn_security_exchange_v1_pb.hrl").

-export([
    new/5,
    sign/2
]).  

-type txn_security_exchange() :: #blockchain_txn_security_exchange_v1_pb{}.
-export_type([txn_security_exchange/0]).

-spec new(libp2p_crypto:pubkey_bin(), libp2p_crypto:pubkey_bin(), non_neg_integer(), non_neg_integer(), non_neg_integer()) -> txn_security_exchange().
new(Payer, Payee, Amount, Fee, Nonce) ->
    #blockchain_txn_security_exchange_v1_pb{
        payer=Payer,
        payee=Payee,
        amount=Amount,
        fee=Fee,
        nonce=Nonce,
        signature= <<>>
    }.

-spec sign(txn_security_exchange(), libp2p_crypto:sig_fun()) -> txn_security_exchange().
sign(Txn, SigFun) ->
    BaseTxn = Txn#blockchain_txn_security_exchange_v1_pb{signature= <<>>},
    EncodedTxn = blockchain_txn_security_exchange_v1_pb:encode_msg(BaseTxn),
    Txn#blockchain_txn_security_exchange_v1_pb{signature=SigFun(EncodedTxn)}.
