-module(oui_txn).

-include_lib("helium_proto/src/pb/helium_txn_oui_v1_pb.hrl").

-export([
    new/4, new/5,
    sign/2,
    sign_payer/2,
    encode/1,
    decode/1
]).  

-type txn_oui() :: #helium_txn_oui_v1_pb{}.
-export_type([txn_oui/0]).

-spec new(libp2p_crypto:pubkey_bin(), [binary()], non_neg_integer(), non_neg_integer()) -> txn_oui().
new(Owner, Addresses, StakingFee, Fee) ->
    #helium_txn_oui_v1_pb{
        owner=Owner,
        addresses=Addresses,
        payer= <<>>,
        staking_fee=StakingFee,
        fee=Fee,
        owner_signature= <<>>,
        payer_signature= <<>>
    }.

-spec new(libp2p_crypto:pubkey_bin(), [binary()], libp2p_crypto:pubkey_bin(), non_neg_integer(), non_neg_integer()) -> txn_oui().
new(Owner, Addresses, Payer, StakingFee, Fee) ->
    #helium_txn_oui_v1_pb{
        owner=Owner,
        addresses=Addresses,
        payer=Payer,
        staking_fee=StakingFee,
        fee=Fee,
        owner_signature= <<>>,
        payer_signature= <<>>
    }.

-spec sign(txn_oui(), libp2p_crypto:sig_fun()) -> txn_oui().
sign(Txn, SigFun) ->
    BaseTxn = Txn#helium_txn_oui_v1_pb{owner_signature= <<>>, payer_signature= <<>>},
    EncodedTxn = helium_txn_oui_v1_pb:encode_msg(BaseTxn),
    Txn#helium_txn_oui_v1_pb{owner_signature=SigFun(EncodedTxn)}.

-spec sign_payer(txn_oui(), libp2p_crypto:sig_fun()) -> txn_oui().
sign_payer(Txn, SigFun) ->
    BaseTxn = Txn#helium_txn_oui_v1_pb{owner_signature= <<>>, payer_signature= <<>>},
    EncodedTxn = helium_txn_oui_v1_pb:encode_msg(BaseTxn),
    Txn#helium_txn_oui_v1_pb{payer_signature=SigFun(EncodedTxn)}.


-spec encode(txn_oui()) -> binary().
encode(Txn) ->
    helium_txn_oui_v1_pb:encode_msg(Txn).

-spec decode(binary()) -> txn_oui().
decode(BinTxn) ->
    helium_txn_oui_v1_pb:decode_msg(BinTxn, helium_txn_oui_v1_pb).