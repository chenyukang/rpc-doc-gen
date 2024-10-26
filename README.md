
# Fiber Network Node RPC

The RPC module provides a set of APIs for developers to interact with FNN. Please note that APIs are not stable yet and may change in the future.

Allowing arbitrary machines to access the JSON-RPC port (using the `rpc.listening_addr` configuration option) is **dangerous and strongly discouraged**. Please strictly limit the access to only trusted machines.

You may refer to the e2e test cases in the `tests/bruno/e2e` directory for examples of how to use the RPC.

<!--**NOTE:** the content below is generated by gen_doc -->

* [RPC Methods](#rpc-methods)


    * [Module Cch](#module-cch)
        * [Method `send_btc`](#cch-send_btc)
        * [Method `receive_btc`](#cch-receive_btc)
        * [Method `get_receive_btc_order`](#cch-get_receive_btc_order)
    * [Module Channel](#module-channel)
        * [Method `open_channel`](#channel-open_channel)
        * [Method `accept_channel`](#channel-accept_channel)
        * [Method `list_channels`](#channel-list_channels)
        * [Method `commitment_signed`](#channel-commitment_signed)
        * [Method `add_tlc`](#channel-add_tlc)
        * [Method `remove_tlc`](#channel-remove_tlc)
        * [Method `shutdown_channel`](#channel-shutdown_channel)
        * [Method `update_channel`](#channel-update_channel)
        * [Method `send_payment`](#channel-send_payment)
        * [Method `get_payment`](#channel-get_payment)
    * [Module Graph](#module-graph)
        * [Method `graph_nodes`](#graph-graph_nodes)
        * [Method `graph_channels`](#graph-graph_channels)
    * [Module Config](#module-config)
    * [Module Mod](#module-mod)
    * [Module Invoice](#module-invoice)
        * [Method `new_invoice`](#invoice-new_invoice)
        * [Method `parse_invoice`](#invoice-parse_invoice)
        * [Method `get_invoice`](#invoice-get_invoice)
    * [Module Info](#module-info)
        * [Method `node_info`](#info-node_info)
    * [Module Peer](#module-peer)
        * [Method `connect_peer`](#peer-connect_peer)
        * [Method `disconnect_peer`](#peer-disconnect_peer)
    * [Module Utils](#module-utils)
* [RPC Types](#rpc-types)

    * [Type `SendBtcParams`](#type-sendbtcparams)
    * [Type `SendBTCResponse`](#type-sendbtcresponse)
    * [Type `ReceiveBtcParams`](#type-receivebtcparams)
    * [Type `GetReceiveBtcOrderParams`](#type-getreceivebtcorderparams)
    * [Type `ReceiveBTCResponse`](#type-receivebtcresponse)
    * [Type `OpenChannelParams`](#type-openchannelparams)
    * [Type `OpenChannelResult`](#type-openchannelresult)
    * [Type `AcceptChannelParams`](#type-acceptchannelparams)
    * [Type `AcceptChannelResult`](#type-acceptchannelresult)
    * [Type `CommitmentSignedParams`](#type-commitmentsignedparams)
    * [Type `ListChannelsParams`](#type-listchannelsparams)
    * [Type `ListChannelsResult`](#type-listchannelsresult)
    * [Type `Channel`](#type-channel)
    * [Type `AddTlcParams`](#type-addtlcparams)
    * [Type `AddTlcResult`](#type-addtlcresult)
    * [Type `RemoveTlcParams`](#type-removetlcparams)
    * [Type `ShutdownChannelParams`](#type-shutdownchannelparams)
    * [Type `UpdateChannelParams`](#type-updatechannelparams)
    * [Type `GetPaymentCommandParams`](#type-getpaymentcommandparams)
    * [Type `GetPaymentCommandResult`](#type-getpaymentcommandresult)
    * [Type `SendPaymentCommandParams`](#type-sendpaymentcommandparams)
    * [Type `GraphNodesParams`](#type-graphnodesparams)
    * [Type `UdtScript`](#type-udtscript)
    * [Type `UdtCellDep`](#type-udtcelldep)
    * [Type `UdtArgInfo`](#type-udtarginfo)
    * [Type `UdtCfgInfos`](#type-udtcfginfos)
    * [Type `NodeInfo`](#type-nodeinfo)
    * [Type `GraphNodesResult`](#type-graphnodesresult)
    * [Type `GraphChannelsParams`](#type-graphchannelsparams)
    * [Type `ChannelInfo`](#type-channelinfo)
    * [Type `GraphChannelsResult`](#type-graphchannelsresult)
    * [Type `RpcConfig`](#type-rpcconfig)
    * [Type `NewInvoiceParams`](#type-newinvoiceparams)
    * [Type `InvoiceResult`](#type-invoiceresult)
    * [Type `ParseInvoiceParams`](#type-parseinvoiceparams)
    * [Type `ParseInvoiceResult`](#type-parseinvoiceresult)
    * [Type `GetInvoiceParams`](#type-getinvoiceparams)
    * [Type `GetInvoiceResult`](#type-getinvoiceresult)
    * [Type `NodeInfoResult`](#type-nodeinforesult)
    * [Type `ConnectPeerParams`](#type-connectpeerparams)
    * [Type `DisconnectPeerParams`](#type-disconnectpeerparams)
## RPC Modules

<a id="cch"></a>
### Module `Cch`

Cch



<a id="send_btc"></a>
#### Method `send_btc`

send_btc

###### Params


* `btc_pay_req` - String
* `currency` - Currency

###### Returns


* `timestamp` - u64
* `expiry` - u64
* `ckb_final_tlc_expiry` - u64
* `currency` - Currency
* `wrapped_btc_type_script` - ckb_jsonrpc_types::Script
* `btc_pay_req` - String
* `ckb_pay_req` - String
* `payment_hash` - String
* `amount_sats` - u128
* `fee_sats` - u128
* `status` - CchOrderStatus

<a id="receive_btc"></a>
#### Method `receive_btc`

receive_btc

###### Params


* `payment_hash` - String
* `channel_id` - Hash256
* `amount_sats` - u128
* `final_tlc_expiry` - u64

###### Returns


* `timestamp` - u64
* `expiry` - u64
* `ckb_final_tlc_expiry` - u64
* `wrapped_btc_type_script` - ckb_jsonrpc_types::Script
* `btc_pay_req` - String
* `payment_hash` - String
* `channel_id` - Hash256
* `tlc_id` - Option
* `amount_sats` - u128
* `fee_sats` - u128
* `status` - CchOrderStatus

<a id="get_receive_btc_order"></a>
#### Method `get_receive_btc_order`

get_receive_btc_order

###### Params


* `payment_hash` - String

###### Returns


* `timestamp` - u64
* `expiry` - u64
* `ckb_final_tlc_expiry` - u64
* `wrapped_btc_type_script` - ckb_jsonrpc_types::Script
* `btc_pay_req` - String
* `payment_hash` - String
* `channel_id` - Hash256
* `tlc_id` - Option
* `amount_sats` - u128
* `fee_sats` - u128
* `status` - CchOrderStatus

<a id="channel"></a>
### Module `Channel`

Channel



<a id="open_channel"></a>
#### Method `open_channel`

open_channel

###### Params


* `peer_id` - PeerId
* `funding_amount` - u128
* `public` - Option
* `funding_udt_type_script` - Option
* `shutdown_script` - Option
* `commitment_delay_epoch` - Option
* `commitment_fee_rate` - Option
* `funding_fee_rate` - Option
* `tlc_locktime_expiry_delta` - Option
* `tlc_min_value` - Option
* `tlc_max_value` - Option
* `tlc_fee_proportional_millionths` - Option
* `max_tlc_value_in_flight` - Option
* `max_tlc_number_in_flight` - Option

###### Returns


* `temporary_channel_id` - Hash256

<a id="accept_channel"></a>
#### Method `accept_channel`

accept_channel

###### Params


* `temporary_channel_id` - Hash256
* `funding_amount` - u128
* `shutdown_script` - Option

###### Returns


* `channel_id` - Hash256

<a id="list_channels"></a>
#### Method `list_channels`

list_channels

###### Params


* `peer_id` - Option

###### Returns


* `channels` - Vec

<a id="commitment_signed"></a>
#### Method `commitment_signed`

commitment_signed

###### Params


* `channel_id` - Hash256

###### Returns


* `` - 

<a id="add_tlc"></a>
#### Method `add_tlc`

add_tlc

###### Params


* `channel_id` - Hash256
* `amount` - u128
* `payment_hash` - Hash256
* `expiry` - LockTime
* `hash_algorithm` - Option

###### Returns


* `tlc_id` - u64

<a id="remove_tlc"></a>
#### Method `remove_tlc`

remove_tlc

###### Params


* `channel_id` - Hash256
* `tlc_id` - u64
* `reason` - RemoveTlcReason

###### Returns


* `` - 

<a id="shutdown_channel"></a>
#### Method `shutdown_channel`

shutdown_channel

###### Params


* `channel_id` - Hash256
* `close_script` - Script
* `force` - Option
* `fee_rate` - u64

###### Returns


* `` - 

<a id="update_channel"></a>
#### Method `update_channel`

update_channel

###### Params


* `channel_id` - Hash256
* `enabled` - Option
* `tlc_locktime_expiry_delta` - Option
* `tlc_minimum_value` - Option
* `tlc_maximum_value` - Option
* `tlc_fee_proportional_millionths` - Option

###### Returns


* `` - 

<a id="send_payment"></a>
#### Method `send_payment`

send_payment

###### Params


* `target_pubkey` - Option
* `amount` - Option
* `payment_hash` - Option
* `final_cltv_delta` - Option
* `invoice` - Option
* `timeout` - Option
* `max_fee_amount` - Option
* `max_parts` - Option
* `keysend` - Option
* `udt_type_script` - Option
* `allow_self_payment` - Option

###### Returns


* `payment_hash` - Hash256
* `status` - PaymentSessionStatus
* `created_at` - u128
* `last_updated_at` - u128
* `failed_error` - Option

<a id="get_payment"></a>
#### Method `get_payment`

get_payment

###### Params


* `payment_hash` - Hash256

###### Returns


* `payment_hash` - Hash256
* `status` - PaymentSessionStatus
* `created_at` - u128
* `last_updated_at` - u128
* `failed_error` - Option

<a id="graph"></a>
### Module `Graph`

Graph



<a id="graph_nodes"></a>
#### Method `graph_nodes`

graph_nodes

###### Params


* `limit` - Option
* `after` - Option

###### Returns


* `nodes` - Vec
* `last_cursor` - JsonBytes

<a id="graph_channels"></a>
#### Method `graph_channels`

graph_channels

###### Params


* `limit` - Option
* `after` - Option

###### Returns


* `channels` - Vec
* `last_cursor` - JsonBytes

<a id="config"></a>
### Module `Config`

Config



<a id="mod"></a>
### Module `Mod`

Mod



<a id="invoice"></a>
### Module `Invoice`

Invoice



<a id="new_invoice"></a>
#### Method `new_invoice`

new_invoice

###### Params


* `amount` - u128
* `description` - Option
* `currency` - Currency
* `payment_preimage` - Hash256
* `expiry` - Option
* `fallback_address` - Option
* `final_cltv` - Option
* `final_htlc_timeout` - Option
* `udt_type_script` - Option
* `hash_algorithm` - Option

###### Returns


* `invoice_address` - String
* `invoice` - CkbInvoice

<a id="parse_invoice"></a>
#### Method `parse_invoice`

parse_invoice

###### Params


* `invoice` - String

###### Returns


* `invoice` - CkbInvoice

<a id="get_invoice"></a>
#### Method `get_invoice`

get_invoice

###### Params


* `payment_hash` - Hash256

###### Returns


* `invoice_address` - String
* `invoice` - CkbInvoice
* `status` - InvoiceStatus

<a id="info"></a>
### Module `Info`

Info



<a id="node_info"></a>
#### Method `node_info`

node_info

###### Params



###### Returns


* `version` - String
* `commit_hash` - String
* `public_key` - Pubkey
* `node_name` - Option
* `peer_id` - PeerId
* `addresses` - Vec
* `chain_hash` - Hash256
* `open_channel_auto_accept_min_ckb_funding_amount` - u64
* `auto_accept_channel_ckb_funding_amount` - u64
* `tlc_locktime_expiry_delta` - u64
* `tlc_min_value` - u128
* `tlc_max_value` - u128
* `tlc_fee_proportional_millionths` - u128
* `channel_count` - u32
* `pending_channel_count` - u32
* `peers_count` - u32
* `network_sync_status` - String
* `udt_cfg_infos` - UdtCfgInfos

<a id="peer"></a>
### Module `Peer`

Peer



<a id="connect_peer"></a>
#### Method `connect_peer`

connect_peer

###### Params


* `address` - MultiAddr
* `save` - Option

###### Returns


* `` - 

<a id="disconnect_peer"></a>
#### Method `disconnect_peer`

disconnect_peer

###### Params


* `peer_id` - PeerId

###### Returns


* `` - 

<a id="utils"></a>
### Module `Utils`

Utils




## RPC Types


