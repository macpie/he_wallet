-module(oui_txn).

-include_lib("helium_proto/include/blockchain_txn_oui_v1_pb.hrl").

-export([
    new/5, new/6,
    sign/2,
    sign_payer/2
]).  

-type txn_oui() :: #blockchain_txn_oui_v1_pb{}.
-export_type([txn_oui/0]).

-spec new(libp2p_crypto:pubkey_bin(), [binary()], non_neg_integer(), non_neg_integer(), non_neg_integer()) -> txn_oui().
new(Owner, Addresses, OUI, StakingFee, Fee) ->
    #blockchain_txn_oui_v1_pb{
        owner=Owner,
        addresses=Addresses,
        oui=OUI,
        payer= <<>>,
        staking_fee=StakingFee,
        fee=Fee,
        owner_signature= <<>>,
        payer_signature= <<>>
    }.

-spec new(libp2p_crypto:pubkey_bin(), [binary()], non_neg_integer(), libp2p_crypto:pubkey_bin(), non_neg_integer(), non_neg_integer()) -> txn_oui().
new(Owner, Addresses, OUI, Payer, StakingFee, Fee) ->
    #blockchain_txn_oui_v1_pb{
        owner=Owner,
        addresses=Addresses,
        oui=OUI,
        payer=Payer,
        staking_fee=StakingFee,
        fee=Fee,
        owner_signature= <<>>,
        payer_signature= <<>>
    }.

-spec sign(txn_oui(), libp2p_crypto:sig_fun()) -> txn_oui().
sign(Txn, SigFun) ->
    BaseTxn = Txn#blockchain_txn_oui_v1_pb{owner_signature= <<>>, payer_signature= <<>>},
    EncodedTxn = blockchain_txn_oui_v1_pb:encode_msg(BaseTxn),
    Txn#blockchain_txn_oui_v1_pb{owner_signature=SigFun(EncodedTxn)}.

-spec sign_payer(txn_oui(), libp2p_crypto:sig_fun()) -> txn_oui().
sign_payer(Txn, SigFun) ->
    BaseTxn = Txn#blockchain_txn_oui_v1_pb{owner_signature= <<>>, payer_signature= <<>>},
    EncodedTxn = blockchain_txn_oui_v1_pb:encode_msg(BaseTxn),
    Txn#blockchain_txn_oui_v1_pb{payer_signature=SigFun(EncodedTxn)}.