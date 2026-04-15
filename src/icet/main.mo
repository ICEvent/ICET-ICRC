import Blob "mo:base/Blob";
import HashMap "mo:base/HashMap";
import Nat "mo:base/Nat";
import Nat64 "mo:base/Nat64";
import Nat8 "mo:base/Nat8";
import Principal "mo:base/Principal";
import Text "mo:base/Text";

actor class ICETToken(initialOwner : Principal, initialSupply : Nat) = this {
  public type Subaccount = Blob;

  public type Account = {
    owner : Principal;
    subaccount : ?Subaccount;
  };

  public type TransferArgs = {
    from_subaccount : ?Subaccount;
    to : Account;
    amount : Nat;
    fee : ?Nat;
    memo : ?Blob;
    created_at_time : ?Nat64;
  };

  public type TransferError = {
    #BadFee : { expected_fee : Nat };
    #BadBurn : { min_burn_amount : Nat };
    #InsufficientFunds : { balance : Nat };
    #TooOld;
    #CreatedInFuture : { ledger_time : Nat64 };
    #Duplicate : { duplicate_of : Nat };
    #TemporarilyUnavailable;
    #GenericError : { error_code : Nat; message : Text };
  };

  public type TransferResult = {
    #Ok : Nat;
    #Err : TransferError;
  };

  public type Value = {
    #Blob : Blob;
    #Text : Text;
    #Nat : Nat;
    #Int : Int;
    #Array : [Value];
    #Map : [(Text, Value)];
  };

  public type SupportedStandard = {
    name : Text;
    url : Text;
  };

  let tokenName : Text = "ICEvent Token";
  let tokenSymbol : Text = "ICET";
  let tokenDecimals : Nat8 = 8;
  let transferFee : Nat = 10_000;

  stable var totalSupply : Nat = initialSupply;
  stable var nextTxIndex : Nat = 0;

  let balances = HashMap.HashMap<Text, Nat>(10, Text.equal, Text.hash);

  func accountKey(account : Account) : Text {
    Principal.toText(account.owner) # ":" # switch (account.subaccount) {
      case (null) "";
      case (?sub) debug_show (Blob.toArray(sub));
    };
  };

  func getBalance(account : Account) : Nat {
    switch (balances.get(accountKey(account))) {
      case (null) 0;
      case (?balance) balance;
    };
  };

  func putBalance(account : Account, value : Nat) {
    balances.put(accountKey(account), value);
  };

  let ownerAccount : Account = {
    owner = initialOwner;
    subaccount = null;
  };

  let _ = balances.put(accountKey(ownerAccount), initialSupply);

  public query func icrc1_name() : async Text { tokenName };

  public query func icrc1_symbol() : async Text { tokenSymbol };

  public query func icrc1_decimals() : async Nat8 { tokenDecimals };

  public query func icrc1_fee() : async Nat { transferFee };

  public query func icrc1_total_supply() : async Nat { totalSupply };

  public query func icrc1_minting_account() : async ?Account { ?ownerAccount };

  public query func icrc1_balance_of(account : Account) : async Nat {
    getBalance(account);
  };

  public query func icrc1_supported_standards() : async [SupportedStandard] {
    [
      {
        name = "ICRC-1";
        url = "https://github.com/dfinity/ICRC-1";
      },
    ];
  };

  public query func icrc1_metadata() : async [(Text, Value)] {
    [
      ("icrc1:name", #Text(tokenName)),
      ("icrc1:symbol", #Text(tokenSymbol)),
      ("icrc1:decimals", #Nat(Nat8.toNat(tokenDecimals))),
      ("icrc1:fee", #Nat(transferFee)),
    ];
  };

  public shared ({ caller }) func icrc1_transfer(args : TransferArgs) : async TransferResult {
    let requestedFee = switch (args.fee) {
      case (null) transferFee;
      case (?fee) fee;
    };

    if (requestedFee != transferFee) {
      return #Err(#BadFee({ expected_fee = transferFee }));
    };

    let fromAccount : Account = {
      owner = caller;
      subaccount = args.from_subaccount;
    };

    let fromBalance = getBalance(fromAccount);
    let debitAmount = args.amount + transferFee;

    if (fromBalance < debitAmount) {
      return #Err(#InsufficientFunds({ balance = fromBalance }));
    };

    let toBalance = getBalance(args.to);

    putBalance(fromAccount, fromBalance - debitAmount);
    putBalance(args.to, toBalance + args.amount);

    if (transferFee > 0 and totalSupply >= transferFee) {
      totalSupply -= transferFee;
    };

    let txIndex = nextTxIndex;
    nextTxIndex += 1;

    #Ok(txIndex);
  };

  public shared ({ caller }) func mint(to : Account, amount : Nat) : async Bool {
    if (caller != initialOwner) {
      return false;
    };

    putBalance(to, getBalance(to) + amount);
    totalSupply += amount;
    true;
  };
};
