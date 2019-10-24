-module(txn).

-include_lib("helium_proto/src/pb/helium_txn_pb.hrl").

-export([
    serialize/1, deserialize/1,
    create_oui_txn/4, create_oui_txn/5
]).


serialize(Txn) ->
    helium_txn_pb:encode_msg(wrap_txn(Txn)).

deserialize(Bin) ->
    unwrap_txn(helium_txn_pb:decode_msg(Bin, helium_txn_pb)).

create_oui_txn(Owner, OwnerSigFun, Payer, Addresses) ->
    Txn = oui_txn:new(Owner, Addresses, Payer, 1, 1),
    SignedTxn = oui_txn:sign(Txn, OwnerSigFun),
    ?MODULE:serialize(SignedTxn).

create_oui_txn(Owner, OwnerSigFun, Payer, PayerSigFun, Addresses) ->
    Txn = oui_txn:new(Owner, Addresses, Payer, 1, 1),
    SignedTxn0 = oui_txn:sign(Txn, OwnerSigFun),
    SignedTxn1 = oui_txn:sign_payer(SignedTxn0, PayerSigFun),
    ?MODULE:serialize(SignedTxn1).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

-spec wrap_txn(any()) -> #helium_txn_pb{}.
wrap_txn(#helium_txn_assert_location_v1_pb{}=Txn) ->
    #helium_txn_pb{txn={assert_location, Txn}};
wrap_txn(#helium_txn_payment_v1_pb{}=Txn) ->
    #helium_txn_pb{txn={payment, Txn}};
wrap_txn(#helium_txn_security_exchange_v1_pb{}=Txn) ->
    #helium_txn_pb{txn={security_exchange, Txn}};
wrap_txn(#helium_txn_create_htlc_v1_pb{}=Txn) ->
    #helium_txn_pb{txn={create_htlc, Txn}};
wrap_txn(#helium_txn_redeem_htlc_v1_pb{}=Txn) ->
    #helium_txn_pb{txn={redeem_htlc, Txn}};
wrap_txn(#helium_txn_add_gateway_v1_pb{}=Txn) ->
    #helium_txn_pb{txn={add_gateway, Txn}};
wrap_txn(#helium_txn_coinbase_v1_pb{}=Txn) ->
    #helium_txn_pb{txn={coinbase, Txn}};
wrap_txn(#helium_txn_security_coinbase_v1_pb{}=Txn) ->
    #helium_txn_pb{txn={security_coinbase, Txn}};
wrap_txn(#helium_txn_consensus_group_v1_pb{}=Txn) ->
    #helium_txn_pb{txn={consensus_group, Txn}};
wrap_txn(#helium_txn_poc_request_v1_pb{}=Txn) ->
    #helium_txn_pb{txn={poc_request, Txn}};
wrap_txn(#helium_txn_poc_receipts_v1_pb{}=Txn) ->
    #helium_txn_pb{txn={poc_receipts, Txn}};
wrap_txn(#helium_txn_gen_gateway_v1_pb{}=Txn) ->
    #helium_txn_pb{txn={gen_gateway, Txn}};
wrap_txn(#helium_txn_oui_v1_pb{}=Txn) ->
    #helium_txn_pb{txn={oui, Txn}};
wrap_txn(#helium_txn_routing_v1_pb{}=Txn) ->
    #helium_txn_pb{txn={routing, Txn}};
wrap_txn(#helium_txn_vars_v1_pb{}=Txn) ->
    #helium_txn_pb{txn={vars, Txn}};
wrap_txn(#helium_txn_rewards_v1_pb{}=Txn) ->
    #helium_txn_pb{txn={rewards, Txn}};
wrap_txn(#helium_txn_token_burn_v1_pb{}=Txn) ->
    #helium_txn_pb{txn={token_burn, Txn}};
wrap_txn(#helium_txn_dc_coinbase_v1_pb{}=Txn) ->
    #helium_txn_pb{txn={dc_coinbase, Txn}};
wrap_txn(#helium_txn_token_burn_exchange_rate_v1_pb{}=Txn) ->
    #helium_txn_pb{txn={token_burn_exchange_rate, Txn}}.

-spec unwrap_txn(#helium_txn_pb{}) -> any().
unwrap_txn(#helium_txn_pb{txn={_, Txn}}) ->
    Txn.